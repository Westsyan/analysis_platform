package controllers

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.inject.Inject

import dao._
import models.Tables
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import utils._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class OtuController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao, otudao: otuDao) extends Controller {

  def toOtuPage(proname: String): Action[AnyContent] = Action { implicit request =>
    val userId = request.session.get("id").head.toInt
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.otuAnalysis.otuPage(allName, proname,request.domain))
  }

  case class otuData(proname: String, otuname: String, sample: Seq[String], minseqlength: Option[Int],
                     otu_radius_pct: Option[String], id: Option[String], strand: String,db:String, method: String, c: Option[String],
                     uma: Option[Int], s: Option[String])

  val otuForm = Form(
    mapping(
      "proname" -> text,
      "otuname" -> text,
      "sample" -> seq(text),
      "minseqlength" -> optional(number),
      "otu_radius_pct" -> optional(text),
      "id" -> optional(text),
      "strand" -> text,
      "db" -> text,
      "method" -> text,
      "c" -> optional(text),
      "uma" -> optional(number),
      "s" -> optional(text)
    )(otuData.apply)(otuData.unapply)
  )

  def saveDeploy = Action { implicit request =>
    val data = otuForm.bindFromRequest.get
    val proname = data.proname
    val otuname = data.otuname
    val ses = getUserIdAndProId(request.session, proname)
    val date = Utils.date
    val row = OtuRow(0, otuname, ses._1, ses._2, date, 0)
    Await.result(otudao.addOtuInfo(Seq(row)), Duration.Inf)
    val otu = Await.result(otudao.getAllByPosition(ses._1, ses._2, otuname), Duration.Inf)
    val otupath = Utils.otuPath(ses._1, ses._2, otu.id)
    new File(otupath).mkdirs()
    val deploy = mutable.Buffer(proname, otuname, data.sample.mkString(","), data.minseqlength.getOrElse(200),
      data.otu_radius_pct.getOrElse(3.0), data.id.getOrElse(0.97), data.strand, data.db , data.method, data.c.getOrElse(0.7),
      data.uma.getOrElse(3), data.s.getOrElse(0.9))
    FileUtils.writeLines(new File(otupath + "/deploy.txt"), deploy.asJava)
    val run = Future{
      runCmd(otu.accountid,otu.projectid,otu.id)
    }
    val json = Json.obj("valid" -> "true", "id" -> otu.id)
    Ok(Json.toJson(json))
  }

  def isRunCmd(id: Int) = Action.async { implicit request =>
    otudao.getById(id).map { x =>
      if (x.state == 0) {
        runCmd(x.accountid, x.projectid, x.id)
      }
      Ok(Json.toJson("success"))
    }
  }

  def runCmd(userId: Int, proId: Int, otuId: Int) = {
    val path = Utils.otuPath(userId, proId, otuId)
    val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val samples = deploy(2).split(",").map { x =>
      val sample = Await.result(sampledao.getByPosition(userId, proId, x), Duration.Inf)
      val fasta = Utils.outPath(userId, proId, sample.id) + "/out_file.fasta"
      x + " " + fasta
    }
    val min = deploy(3)
    val otupct = deploy(4)
    val id = deploy(5)
    val strand = deploy(6)

    val command1 = s"perl ${Utils.toolPath}/otu/otu_cls.pl ${samples.mkString(" ")} " +
      s"-output  ${path}/otu_table.txt -rep_seq  ${path}/otu_rep_seqs.fasta -minseqlength ${min} " +
      s"-otu_radius_pct ${otupct} -id ${id} -strand ${strand}"

    val command2 = RdpCmd(path)
    val command = new ExecCommand
    val tmp = path + "/tmp"
    new File(tmp).mkdir()
    command.exec(command1, command2, tmp)
    if (command.isSuccess) {
      Await.result(otudao.updateState(otuId, 1), Duration.Inf)
      val tax = FileUtils.readLines(new File(path, "/tmp/otu_rep_seqs_tax_assignments.txt")).asScala
      val taxs = tax.map(_.split("\t").take(2).mkString("\t"))
      FileUtils.writeLines(new File(path, "tax_assign.txt"), taxs.asJava)
      FileUtils.writeStringToFile(new File(path, "log.txt"), "运行成功")
      FileUtils.deleteDirectory(new File(tmp))
    } else {
      if (new File(path).exists()) {
        Await.result(otudao.updateState(otuId, 2), Duration.Inf)
        val log = command.getErrStr.split("00:").mkString("\n00:")
        FileUtils.writeStringToFile(new File(path, "log.txt"), log)
        FileUtils.deleteDirectory(new File(tmp))
      }
    }
  }

  def RdpCmd(path: String): String = {
    val data = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val command = if (data(8) == "rdp") {
      s"assign_taxonomy.py -t ${Utils.toolPath}/otu/Taxonomy/${data(7)}.tax -r ${Utils.toolPath}/otu/Taxonomy/${data(7)}.fasta -i ${path}/otu_rep_seqs.fasta " +
        s"-o ${path}/tmp -m rdp --rdp_max_memory=50000 -c ${data(9)}"
    } else {
      s"assign_taxonomy.py -t ${Utils.toolPath}/otu/Taxonomy/${data(7)}.tax -r ${Utils.toolPath}/otu/Taxonomy/${data(7)}.fasta -i ${path}/otu_rep_seqs.fasta " +
        s"-o ${path}/tmp -m uclust --similarity=${data(11)} --uclust_max_accepts=${data(10)}"
    }
    command
  }

  def otuPage(proname: String) = Action { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val allName = Await.result(projectdao.getAllProject(ses._1), Duration.Inf)
    Ok(views.html.otuAnalysis.task(allName, proname,request.domain))
  }

  def getUserIdAndProId(session: Session, proname: String): (Int, Int) = {
    val userId = session.get("id").head.toInt
    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    (userId, proId)
  }

  def getAllId(session: Session, proname: String, sample: String): (Int, Int, Int) = {
    val userId = session.get("id").head.toInt

    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    val sampleId = Await.result(sampledao.getIdByPosition(userId, proId, sample), Duration.Inf)
    (userId, proId, sampleId)
  }

  def getAllTask(proname: String) = Action { implicit request =>
    val json = dealWithSample(proname, request.session)
    Ok(Json.toJson(json))
  }

  def download(id: Int, code: Int) = Action { implicit request =>
    val row = Await.result(otudao.getById(id), Duration.Inf)
    val path = Utils.otuPath(row.accountid, row.projectid, id)
    val (file, name) = if (code == 1) {
      (new File(path, "otu_rep_seqs.fasta"), "otu_rep_seqs.fasta")
    } else if (code == 2) {
      (new File(path, "otu_table.txt"), "otu_table.txt")
    } else {
      (new File(path, "tax_assign.txt"), "tax_assign.txt")
    }
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + name),
      CONTENT_TYPE -> "application/x-download"
    )
  }

  def dealWithSample(proname: String, session: Session) = {
    val id = getUserIdAndProId(session, proname)
    val otus = Await.result(otudao.getAllOtuByPosition(id._1, id._2), Duration.Inf)
    val json = otus.sortBy(_.id).reverse.map { x =>
      val otuname = x.otuname
      val date = x.createdata.toLocalDate
      val state = if (x.state == 0) {
        "正在运行 <img src='/assets/images/timg.gif'  style='width: 20px; height: 20px;'><input class='state' value='" + x.state + "'>"
      } else if (x.state == 1) {
        "成功<input class='state' value='" + x.state + "'>"
      } else {
        "失败<input class='state' value='" + x.state + "'>"
      }
      val otuSeqs = if (x.state == 1) {
        getOtuSeqs(x).toString
      } else {
        "NA"
      }
      val results = if (x.state == 1) {
        s"""
           |<a class="fastq" href="/project/downloadOtu?id=${x.id}&code=1" title="OTU代表序列"><b>otu_rep_seqs.fasta</b></a>,&nbsp;
           |<a class="fastq" href="/project/downloadOtu?id=${x.id}&code=2" title="OTU丰度信息表"><b>otu_table.txt</b></a>,&nbsp;
           |<a class="fastq" href="/project/downloadOtu?id=${x.id}&code=3" title="分类学注释结果"><b>tax_assign.txt</b></a>
           |<button class="update" onclick="openTable(this)"  id="${x.id}" title="OTU Taxon Table"><i class="fa fa-eye"></i></button>
           """.stripMargin
      } else {
        ""
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="restart(this)" value="${x.id}" title="重新运行OTU聚类和分类学注释"><i class="fa fa-repeat"></i></button>
           |  <button class="update" onclick="openRdp(this)" value="${x.id}" title="重新运行分类学注释"><i class="fa fa-rotate-left"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.otuname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.otuname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           |<button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.otuname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>"""
      }
      Json.obj("otuname" -> otuname, "state" -> state, "createdate" -> date, "results" -> results, "operation" -> operation, "otuseqs" -> otuSeqs)
    }
    json

  }

  def getOtuSeqs(row: Tables.OtuRow): Int = {
    val path = Utils.otuPath(row.accountid, row.projectid, row.id)
    val buffer = FileUtils.readLines(new File(path, "otu_table.txt")).asScala
    val seqs = buffer.size - 1
    seqs
  }

  def deleteTask(id: Int) = Action.async { implicit request =>
    otudao.getById(id).flatMap { x =>
      otudao.deleteOtu(id).map { y =>
        val run = Future{
          val path = Utils.otuPath(x.accountid, x.projectid, id)
          FileUtils.deleteDirectory(new File(path))
        }
        Ok(Json.toJson("success"))
      }
    }

  }

  def getTime = Action { implicit request =>
    val now = new Date()
    val dateFormat = new SimpleDateFormat("yyMMddHHmmss")
    val date = dateFormat.format(now)
    Ok(Json.obj("date" -> date))

  }

  def getLog(id: Int) = Action { implicit request =>
    val row = Await.result(otudao.getById(id), Duration.Inf)
    val path = Utils.otuPath(row.accountid, row.projectid, row.id)
    val log = FileUtils.readLines(new File(path, "log.txt")).asScala
    var html =
      """
        |<style>
        |   .logClass{
        |       font-size : 16px;
        |       font-weight:normal;
        |   }
        |</style>
      """.stripMargin
    html += "<b class='logClass'>" + log.mkString("</b><br><b class='logClass'>") + "</b>"
    val json = Json.obj("log" -> html)
    Ok(Json.toJson(json))

  }

  def getDeploy(id: Int) = Action.async { implicit request =>
    val deploy = getDeployInfo(id)

    val sample = deploy(2).split(",")
    otudao.getById(id).flatMap { x =>
      val userId = x.accountid
      val proId = x.projectid
      sampledao.checkByPosition(userId, proId, deploy(2)).map { y =>
        val (valid, message) = if (sample.size == y.size) {
          ("true", "success")
        } else {
          val validSample = y.map(_.sample)
          val s = sample.diff(validSample)
          ("false", "样品" + s.mkString(",") + "已被删除")
        }
        val json = Json.obj("sample" -> deploy(2), "min" -> deploy(3), "otupct" -> deploy(4), "id" -> deploy(5),
          "strand" -> deploy(6),"db" -> deploy(7), "method" -> deploy(8), "c" -> deploy(9), "uma" -> deploy(10), "s" -> deploy(11),
          "valid" -> valid, "message" -> message)
        Ok(Json.toJson(json))

      }
    }

  }

  def getRdpDeploy(id: Int) = Action { implicit request =>
    val row = Await.result(otudao.getById(id), Duration.Inf)
    val deploy = getDeployInfo(id)
    val json = Json.obj("otuname" -> row.otuname, "c" -> deploy(9), "db" -> deploy(7) ,"method" -> deploy(8),
      "uma" -> deploy(10), "s" -> deploy(11))
    Ok(Json.toJson(json))
  }

  case class RdpData(rdp_id: Int, method_2: String, db_2 : String,rdp_c: String, rdp_uma: Int, rdp_s: String)

  val rdpForm = Form(
    mapping(
      "rdp_id" -> number,
      "method_2" -> text,
      "db_2" -> text,
      "rdp_c" -> text,
      "rdp_uma" -> number,
      "rdp_s" -> text
    )(RdpData.apply)(RdpData.unapply)
  )

  def prepareRdp = Action.async { implicit request =>
    val data = rdpForm.bindFromRequest.get
    val id = data.rdp_id
    otudao.getById(id).flatMap { x =>
      val path = Utils.otuPath(x.accountid, x.projectid, x.id)
      val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val c = data.rdp_c
      val d = mutable.Buffer(deploy(0), deploy(1), deploy(2), deploy(3), deploy(4), deploy(5), deploy(6),data.db_2, data.method_2, c, data.rdp_uma, data.rdp_s)
      new File(path, "deploy.txt").delete()
      FileUtils.writeLines(new File(path, "deploy.txt"), d.asJava)
      otudao.updateState(x.id, 0).map { y =>
        Ok(Json.obj("valid" -> "true", "id" -> x.id))
      }
    }
  }

  def runRdpCmd(id: Int) = Action { implicit request =>
    val x = Await.result(otudao.getById(id), Duration.Inf)
    val path = Utils.otuPath(x.accountid, x.projectid, x.id)
    val command1 = RdpCmd(path)
    val command = new ExecCommand
    val tmp = path + "/tmp"
    new File(tmp).mkdir()
    command.exe(command1, tmp)
    if (command.isSuccess) {
      Await.result(otudao.updateState(id, 1), Duration.Inf)
      val tax = FileUtils.readLines(new File(path, "/tmp/otu_rep_seqs_tax_assignments.txt")).asScala
      val taxs = tax.map(_.split("\t").take(2).mkString("\t"))
      FileUtils.writeLines(new File(path, "tax_assign.txt"), taxs.asJava)
      FileUtils.deleteDirectory(new File(tmp))
      Ok(Json.obj("valid" -> "true"))
    } else {
      Await.result(otudao.updateState(id, 2), Duration.Inf)
      println(command.getErrStr)
      FileUtils.deleteDirectory(new File(tmp))
      Ok(Json.obj("valid" -> "false"))
    }
  }

  def getDeployInfo(id: Int): mutable.Buffer[String] = {
    val x = Await.result(otudao.getById(id), Duration.Inf)
    val path = Utils.otuPath(x.accountid, x.projectid, x.id)
    val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    deploy
  }

  case class updateOtunameData(otuId: Int, newotuname: String)

  val updateOtunameForm = Form(
    mapping(
      "otuId" -> number,
      "newotuname" -> text
    )(updateOtunameData.apply)(updateOtunameData.unapply)
  )

  def updateOtuName = Action.async { implicit request =>
    val data = updateOtunameForm.bindFromRequest.get
    val id = data.otuId
    val name = data.newotuname
    otudao.updateOtuName(id, name).map { x =>
      Ok(Json.obj("valid" -> "true"))
    }
  }

  case class resetData(otuIds: Int,  minseqlength: Option[Int], otu_radius_pct: Option[String], id: Option[String],
                       strand: String,db_1: String, method_1: String, c: Option[String],
                       uma: Option[Int], s: Option[String])

  val resetForm = Form(
    mapping(
      "otuIds" -> number,
      "minseqlength" -> optional(number),
      "otu_radius_pct" -> optional(text),
      "id" -> optional(text),
      "strand" -> text,
      "db_1" -> text,
      "method_1" -> text,
      "c" -> optional(text),
      "uma" -> optional(number),
      "s" -> optional(text)
    )(resetData.apply)(resetData.unapply)
  )

  def resetOtu = Action.async { implicit request =>
    val data = resetForm.bindFromRequest.get
    val otuid = data.otuIds
    otudao.getById(otuid).flatMap { x =>
      val path = Utils.otuPath(x.accountid, x.projectid, x.id)
      val buffer = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val b = mutable.Buffer(buffer.head, buffer(1), buffer(2), data.minseqlength.getOrElse(200),
        data.otu_radius_pct.getOrElse(3.0), data.id.getOrElse(0.97), data.strand, data.db_1,data.method_1, data.c.getOrElse(0.7),
        data.uma.getOrElse(3), data.s.getOrElse(0.9))
      new File(path, "deploy.txt").delete()
      FileUtils.writeLines(new File(path, "deploy.txt"), b.asJava)
      otudao.updateState(x.id, 0).map { y =>
        Ok(Json.obj("valid" -> "true", "id" -> x.id))
      }
    }
  }

  def runResetCmd(id: Int) = Action.async { implicit request =>
    otudao.getById(id).map { x =>
      runCmd(x.accountid, x.projectid, x.id)
      Ok(Json.toJson("success"))
    }
  }

  case class resetRdpData(rdpId: Int, db: String, c: String)

  val resetRdpForm = Form(
    mapping(
      "rdpId" -> number,
      "db" -> text,
      "c" -> text
    )(resetRdpData.apply)(resetRdpData.unapply)
  )

  def resetRdp = Action.async { implicit request =>
    val data = resetRdpForm.bindFromRequest.get
    val id = data.rdpId
    otudao.getById(id).flatMap { x =>
      val path = Utils.otuPath(x.accountid, x.projectid, x.id)
      val buffer = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val b = mutable.Buffer(buffer(0), buffer(1), buffer(2), buffer(3), buffer(4), buffer(5), data.db, data.c)
      new File(path, "deploy.txt").delete()
      FileUtils.writeLines(new File(path, "deploy.txt"), b.asJava)
      otudao.updateState(x.id, 0).map { y =>
        Ok(Json.obj("valid" -> "true", "id" -> x.id))
      }
    }
  }

  def runResetRdpCmd(id: Int) = Action.async { implicit request =>
    otudao.getById(id).map { x =>
      val path = Utils.otuPath(x.accountid, x.projectid, x.id)
      val command1 = RdpCmd(path)
      val command = new ExecCommand
      command.exe(command1, path + "/tmp")
      if (command.isSuccess) {
        println(command.getErrStr)
        Ok(Json.toJson("success"))
      } else {
        Ok(Json.toJson("success"))
      }
    }
  }

  case class checkNewnameData(newotuname: String)

  val checkNewnameForm = Form(
    mapping(
      "newotuname" -> text
    )(checkNewnameData.apply)(checkNewnameData.unapply)
  )

  def checkOtuname(proname: String) = Action.async { implicit request =>
    val data = checkNewnameForm.bindFromRequest.get
    val ses = getUserIdAndProId(request.session, proname)
    otudao.getAllByPosi(ses._1, ses._2, data.newotuname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  case class checkOtunameData(otuname: String)

  val checkOtunameForm = Form(
    mapping(
      "otuname" -> text
    )(checkOtunameData.apply)(checkOtunameData.unapply)
  )

  def checkName(proname: String) = Action.async { implicit request =>
    val data = checkOtunameForm.bindFromRequest.get
    val ses = getUserIdAndProId(request.session, proname)
    otudao.getAllByPosi(ses._1, ses._2, data.otuname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  def getTax(id: Int) = Action { implicit request =>
    val row = Await.result(otudao.getById(id), Duration.Inf)
    val path = Utils.otuPath(row.accountid, row.projectid, row.id)
    val table = FileUtils.readLines(new File(path, "otu_table.txt")).asScala
    val tax = FileUtils.readLines(new File(path, "tax_assign.txt")).asScala
    val head = table.head + "\t" + "taxonomy"

    val z = table.drop(1).map(_.split("\t")).map { x =>
      tax.map(_.split("\t")).map { y =>
        if (x(0) == y(0)) {
          x.mkString("\t") + "\t" + y(1)
        } else {
          "null"
        }
      }.distinct.diff(Array("null")).head
    }
    val json =getDataJson(head +: z)


    Ok(Json.toJson(json))
  }


  def getDataJson(lines: mutable.Buffer[String]) = {
    val sampleNames = lines.head.split("\t").drop(1)
    val array = lines.drop(1).map { line =>
      val columns = line.split("\t")
      val map = mutable.Map[String, String]()
      map += ("otu_id" -> columns(0))
      sampleNames.zip(columns.drop(1)).foreach { case (sampleName, data) =>
        map += (sampleName -> data)
      }
      map
    }
    Json.obj("array" -> array, "sampleNames" -> sampleNames)
  }


}
