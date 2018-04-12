package controllers

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date
import javax.inject.Inject

import dao._
import models.Tables._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import utils._

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class OtuController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao, otudao: otuDao) extends Controller {

  def toOtuPage(proname: String): Action[AnyContent] = Action { implicit request =>
    val account = request.session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.otuAnalysis.otuPage(allName, proname))
  }

  case class otuData(proname: String, otuname: String, sample: Seq[String], minseqlength: Int,
                     otu_radius_pct: String, id: String, strand:String, method: String, c: String,uma:Int,s:String)

  val otuForm = Form(
    mapping(
      "proname" -> text,
      "otuname" -> text,
      "sample" -> seq(text),
      "minseqlength" -> number,
      "otu_radius_pct" -> text,
      "id" -> text,
      "strand" -> text,
      "method" -> text,
      "c" -> text,
      "uma" -> number,
      "s" -> text
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

    val deploy = mutable.Buffer(proname, otuname, data.sample.mkString(","), data.minseqlength.toString,
      data.otu_radius_pct, data.id, data.strand, data.method, data.c, data.uma, data.s)
    FileUtils.writeLines(new File(otupath + "/deploy.txt"), deploy.asJava)
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
    println(command1)
    val command2 = RdpCmd(path)
    val command = new ExecCommand
    val tmp = path + "/tmp"
    new File(tmp).mkdir()
    command.exec(command1, command2, tmp)
    if (command.isSuccess) {
      Await.result(otudao.updateState(otuId, 1), Duration.Inf)
      val log = command.getErrStr.split("00:").mkString("\n00:").split("\n").toBuffer
      FileUtils.writeLines(new File(path, "log.txt"), log.asJava)
      FileUtils.deleteDirectory(new File(tmp))
    } else {
      Await.result(otudao.updateState(otuId, 2), Duration.Inf)
      val log = command.getErrStr.split("00:").mkString("\n00:")
      FileUtils.writeStringToFile(new File(path, "log.txt"), log)
      FileUtils.deleteDirectory(new File(tmp))
    }
  }

  def RdpCmd(path: String): String = {
    val data = FileUtils.readLines(new File(path, "deploy.txt")).asScala
    val command = if(data(7) == "rdp"){
      s"perl ${Utils.toolPath}/otu/utax.pl -input ${path}/otu_rep_seqs.fasta -cutoff ${data(7)} " +
        s"-qiime_output ${path}/tax_assign.txt -rdp_output ${path}/rdp_format_out.txt -db " +
        s"${Utils.toolPath}/otu/utaxref/${data(6)}_full_refdb.udb"
    }else{
      s"perl ${Utils.toolPath}/otu/utax.pl ${path}/otu_rep_seqs.fasta -db silva.16s -m uclust -o " +
        s"-qiime_output ${path}/tax_assign.txt -rdp_output ${path}/rdp_format_out.txt -db "
    }

    command
  }

  def otuPage(proname: String, id: Int) = Action { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val allName = Await.result(projectdao.getAllProject(ses._1), Duration.Inf)
    val samples = Await.result(sampledao.getAllSample(ses._1, ses._2), Duration.Inf)

    Ok(views.html.otuAnalysis.task(samples, allName, proname, id))
  }

  def getUserIdAndProId(session: Session, proname: String): (Int, Int) = {
    val account = session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    (userId, proId)
  }

  def getAllId(session: Session, proname: String, sample: String): (Int, Int, Int) = {
    val account = session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
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
      val results = if (x.state == 1) {
        s"""
           |<a class="fastq" href="/project/downloadOtu?id=${x.id}&code=1" title="OTU代表序列文件"><b>otu_rep_seqs.fasta</b></a>,&nbsp;
           |<a class="fastq" href="/project/downloadOtu?id=${x.id}&code=2" title="OTU Table表格文件"><b>otu_table.txt</b></a>,&nbsp;
           |<a class="fastq" href="/project/downloadOtu?id=${x.id}&code=3" title="Qiime格式的分类学注释结果文件"><b>tax_assign.txt</b></a>
           """.stripMargin
      } else {
        ""
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="updateOtu(this)" value="${x.otuname}" id="${x.id}" title="修改样品名"><i class="fa fa-pencil"></i></button>
           |  <button class="update" onclick="restart(this)" value="${x.id}" title="重新运行"><i class="fa fa-repeat"></i></button>
           |  <button class="update" onclick="openRdp(this)" value="${x.id}" title="重新运行RDP"><i class="fa fa-rotate-left"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.otuname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.otuname}" id="${x.id}" title="删除任务"><i class="fa fa-trash"></i></button>
           |<button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        ""
      }
      Json.obj("otuname" -> otuname, "state" -> state, "createdate" -> date, "results" -> results, "operation" -> operation)
    }
    json

  }

  def deleteTask(id: Int) = Action.async { implicit request =>
    otudao.getById(id).flatMap { x =>
      val path = Utils.otuPath(x.accountid, x.projectid, id)
      FileUtils.deleteDirectory(new File(path))
      otudao.deleteOtu(id).map { y =>
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

  def getDeploy(id: Int) = Action { implicit request =>
    val deploy = getDeployInfo(id)
    val json = Json.obj("sample" -> deploy(2), "min" -> deploy(3), "otupct" -> deploy(4),
                        "id" -> deploy(5), "c" -> deploy(7), "db" -> deploy(6))
    Ok(Json.toJson(json))
  }

  def getRdpDeploy(id: Int) = Action { implicit request =>
    val row = Await.result(otudao.getById(id), Duration.Inf)
    val deploy = getDeployInfo(id)
    val db = deploy(6)
    val c = deploy(7)
    val json = Json.obj("otuname" -> row.otuname, "c" -> c, "db" -> db)
    Ok(Json.toJson(json))
  }

  case class RdpData(rdp_id: Int, rdp_db: String, rdp_c: String)

  val rdpForm = Form(
    mapping(
      "rdp_id" -> number,
      "rdp_db" -> text,
      "rdp_c" -> text
    )(RdpData.apply)(RdpData.unapply)
  )

  def prepareRdp = Action.async { implicit request =>
    val data = rdpForm.bindFromRequest.get
    val id = data.rdp_id
      otudao.getById(id).flatMap { x =>
        val path = Utils.otuPath(x.accountid, x.projectid, x.id)
        val deploy = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val c = data.rdp_c
      val db = data.rdp_db
      val d = mutable.Buffer(deploy(0),deploy(1),deploy(2),deploy(3),deploy(4),deploy(5),db,c)
      new File(path, "deploy.txt").delete()
      FileUtils.writeLines(new File(path, "deploy.txt"), d.asJava)
      otudao.updateState(x.id, 0).map { y =>
        Ok(Json.obj("valid" -> "true", "id" -> x.id))
      }
    }
  }

  def runRdpCmd(id:Int) = Action{implicit request=>
      val x = Await.result(otudao.getById(id),Duration.Inf)
      val path = Utils.otuPath(x.accountid,x.projectid,x.id)
      val command1 = RdpCmd(path)
      val command = new ExecCommand
      val tmp = path + "/tmp"
      new File(tmp).mkdir()
      command.exe(command1, tmp)
      if(command.isSuccess){
        Await.result(otudao.updateState(id, 1), Duration.Inf)
        println(command.getErrStr)
        println("---------------分割-----------------")
        println(command.getOutStr)
        new File(tmp).delete()
        Ok(Json.obj("valid" -> "true"))
      }else{
        Await.result(otudao.updateState(id, 2), Duration.Inf)
        println(command.getErrStr)
        new File(tmp).delete()
        Ok(Json.obj("valid" -> "false"))
      }
    }

  def getDeployInfo(id:Int) : mutable.Buffer[String] = {
      val x = Await.result(otudao.getById(id),Duration.Inf)
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

  case class resetData(otuIds: Int, minseqlength: Int, otu_radius_pct: String, id: String, db: String, c: String)

  val resetForm = Form(
    mapping(
      "otuIds" -> number,
      "minseqlength" -> number,
      "otu_radius_pct" -> text,
      "id" -> text,
      "db" -> text,
      "c" -> text
    )(resetData.apply)(resetData.unapply)
  )

  def resetOtu = Action.async { implicit request =>
    val data = resetForm.bindFromRequest.get
    val otuid = data.otuIds
    otudao.getById(otuid).flatMap { x =>
      val path = Utils.otuPath(x.accountid, x.projectid, x.id)
      val buffer = FileUtils.readLines(new File(path, "deploy.txt")).asScala
      val b = mutable.Buffer(buffer(0), buffer(1), buffer(2), data.minseqlength, data.otu_radius_pct, data.id, data.db, data.c)
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

}
