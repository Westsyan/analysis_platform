package controllers

import java.io.File
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


class SampleController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao) extends Controller {

  def enterHome(projectname: String): Action[AnyContent] = Action { implicit request =>
    val account = request.session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val projectId = Await.result(projectdao.getIdByProjectname(userId, projectname), Duration.Inf)
    val data = new File(Utils.path + "/" + userId + "/" + projectId)
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    if (data.listFiles().size < 2) {
      Redirect(routes.SampleController.loadData(projectname))
    } else {
      Redirect(routes.SampleController.dataPage(projectname, ""))
    }
  }

  def loadData(proname: String): Action[AnyContent] = Action { implicit request =>
    val account = request.session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val allName = Await.result(projectdao.getAllProject(userId), Duration.Inf)
    Ok(views.html.fileupload.uploadFile(allName, proname))
  }

  def home = Action { implicit request =>
    val account = request.session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val all = Await.result(projectdao.getAll(userId), Duration.Inf)
    val projectname = all.map(_.projectname)
    Ok(views.html.background.home(all, projectname)).withNewSession.withSession("user" -> account)
  }

  case class paraData(proname: String, sample: String, encondingType: String, stepMethod: String, adapter: Option[String],
                      seed_mismatches: Option[Int], palindrome_clip_threshold: Option[Int],
                      simple_clip_threshold: Option[Int], trimMethod: String, window_size: Option[Int],
                      required_quality: Option[Int], minlenMethod: String, minlen: Option[Int],
                      leadingMethod: String, leading: Option[Int], trailingMethod: String, trailing: Option[Int],
                      cropMethod: String, crop: Option[Int], headcropMethod: String, headcrop: Option[Int])


  val paraForm = Form(
    mapping(
      "proname" -> text,
      "sample" -> text,
      "encondingType" -> text,
      "stepMethod" -> text,
      "adapter" -> optional(text),
      "seed_mismatches" -> optional(number),
      "palindrome_clip_threshold" -> optional(number),
      "simple_clip_threshold" -> optional(number),
      "trimMethod" -> text,
      "window_size" -> optional(number),
      "required_quality" -> optional(number),
      "minlenMethod" -> text,
      "minlen" -> optional(number),
      "leadingMethod" -> text,
      "leading" -> optional(number),
      "trailingMethod" -> text,
      "trailing" -> optional(number),
      "cropMethod" -> text,
      "crop" -> optional(number),
      "headcropMethod" -> text,
      "headcrop" -> optional(number)
    )(paraData.apply)(paraData.unapply)
  )

  case class flashData(m: Int, M: Int, x: String)

  def flashForm = Form(
    mapping(
      "m" -> number,
      "M" -> number,
      "x" -> text
    )(flashData.apply)(flashData.unapply)
  )

  def uploadFile = Action(parse.multipartFormData) { implicit request =>
    val path = Utils.path
    val account = request.session.get("user").head
    val file = request.body.files
    val paradata = paraForm.bindFromRequest.get
    val flashdata = flashForm.bindFromRequest.get
    val proname = paradata.proname
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val project = Await.result(projectdao.getProject(userId, proname), Duration.Inf)
    val type1 = paradata.encondingType
    val type2 = type1.split("-phred").mkString
    val sample = paradata.sample
    val date = Utils.date
    val sa = SampleRow(0, sample, userId, project.id, date, 0, 0)
    Await.result(sampledao.addSample(Seq(sa)), Duration.Inf)
    val sampleId = Await.result(sampledao.getIdByPosition(userId, project.id, sample), Duration.Inf)
    val outPath = Utils.outPath(userId,project.id,sampleId)
    val in1 = file(0).ref.file.getPath
    val name1 = file(0).filename
    val in2 = file(1).ref.file.getPath
    val name2 = file(1).filename
    val out1 = outPath + "/raw.data_1.fastq"
    val out2 = outPath + "/raw.data_2.fastq"
    getFastq(in1, out1,name1)
    getFastq(in2, out2,name2)
    new File(outPath + "/tmp").mkdir()
    val deploy = mutable.Buffer(sampleId, type1, paradata.stepMethod, paradata.adapter.get, paradata.seed_mismatches.get,
      paradata.palindrome_clip_threshold.get, paradata.simple_clip_threshold.get, paradata.trimMethod,
      paradata.window_size.get, paradata.required_quality.get, paradata.minlenMethod, paradata.minlen.get,
      paradata.leadingMethod, paradata.leading.get, paradata.trailingMethod, paradata.trailing.get,
      paradata.cropMethod, paradata.crop.get, paradata.headcropMethod, paradata.headcrop.get, type2,
      flashdata.m, flashdata.M, flashdata.x)

    FileUtils.writeLines(new File(outPath + "/deploy.txt"), deploy.asJava)
    Ok(Json.obj("valid" -> "true"))
  }

  def reset = Action { implicit request =>
    val path = Utils.path
    val account = request.session.get("user").head
    val paradata = paraForm.bindFromRequest.get
    val flashdata = flashForm.bindFromRequest.get
    val proId = paradata.proname.toInt
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val type1 = paradata.encondingType
    val type2 = type1.split("-phred").mkString
    val sample = paradata.sample
    val sampleAll = Await.result(sampledao.getByPosition(userId, proId, sample), Duration.Inf)
    val sampleId = sampleAll.id
    val date = sampleAll.createdata
    val sa = SampleRow(sampleId, sample, userId, proId, date, 0, 0)
    Await.result(sampledao.update(sa), Duration.Inf)
    val outPath = path + "/" + userId + "/" + proId + "/" + sampleId
    new File(outPath + "/tmp").mkdir()
    val deploy = mutable.Buffer(sampleId, type1, paradata.stepMethod, paradata.adapter.get, paradata.seed_mismatches.get,
      paradata.palindrome_clip_threshold.get, paradata.simple_clip_threshold.get, paradata.trimMethod,
      paradata.window_size.get, paradata.required_quality.get, paradata.minlenMethod, paradata.minlen.get,
      paradata.leadingMethod, paradata.leading.get, paradata.trailingMethod, paradata.trailing.get,
      paradata.cropMethod, paradata.crop.get, paradata.headcropMethod, paradata.headcrop.get, type2,
      flashdata.m, flashdata.M, flashdata.x)
    FileUtils.writeLines(new File(outPath + "/deploy.txt"), deploy.asJava)
    Ok(Json.obj("valid" -> "true"))
  }

  def runCmd1(sampleId: Int, proname: String, session: Session) = {
    val ses = getUserIdAndProId(session, proname)
    val outPath = Utils.outPath(ses._1, ses._2, sampleId)
    val deploy = FileUtils.readLines(new File(outPath, "deploy.txt")).asScala
    val command1 = dealWithPara1(outPath, deploy)
    val command2 = dealWithFlash1(outPath, deploy)
    val command = new ExecCommand
    command.exec(command1, command2)
    val samples = Await.result(sampledao.getAllSample(ses._1, ses._2), Duration.Inf)
    Await.result(projectdao.updateCount(ses._2, samples.size), Duration.Inf)
    val row = Await.result(sampledao.getAllById(sampleId), Duration.Inf)
    if (command.isSuccess) {
      FileUtils.deleteDirectory(new File(outPath + "/tmp"))
      val seqs = getSeqs(outPath)
      val sampleRow = SampleRow(sampleId, row.sample, ses._1, ses._2, row.createdata, seqs, 1)
      Await.result(sampledao.update(sampleRow), Duration.Inf)
      getLog(outPath, command.getErrStr)
    } else {
      val sampleRow = SampleRow(sampleId, row.sample, ses._1, ses._2, row.createdata, 0, 2)
      Await.result(sampledao.update(sampleRow), Duration.Inf)
      getLog(outPath, command.getErrStr)
      println("fail")
    }
  }

  def getFastq(path: String, outputPath: String,name:String): Unit = {
    val suffix = name.split('.').last
    if (suffix == "gz") {
      FileUtils.writeStringToFile(new File(outputPath),"")
      CompactAlgorithm.unGzipFile(path, outputPath)
    } else {
      FileUtils.copyFile(new File(path), new File(outputPath))
    }
  }

  def getSeqs(outPath: String): Int = {
    val fastq = outPath + "/out.extendedFrags.fastq"
    val fasta = outPath + "/out_file.fasta"
    fastqToFasta.fqToFa(fastq, fasta)
    val seq = FileUtils.readLines(new File(fasta)).asScala
    new File(fastq).delete()
    val seqs = seq.size / 2
    seqs
  }

  def getLog(outPath: String, output: String): Unit = {
    val flashFile = new File(outPath, "flash_log.txt")
    val trans = output.split("Input Read Pairs")
    val input = ("Input Read Pairs" + trans.drop(1).head).split("Both Surviving")
    val both = ("Both Surviving" + input.drop(1).head).split("Forward Only Surviving")
    val forward = ("Forward Only Surviving" + both.drop(1).head).split("Reverse Only Surviving")
    val reverse = ("Reverse Only Surviving" + forward.drop(1).head).split("Dropped")
    val drop = ("Dropped" + reverse.drop(1).head).split("TrimmomaticPE")
    val PE = "TrimmomaticPE" + drop.drop(1).head
    val tri = mutable.Buffer(input.head, both.head, forward.head, reverse.head, drop.head, PE, "\n")
    val flash = FileUtils.readLines(flashFile).asScala
    val f = flash.map(_.split(""))
    val fs = f.map{x=>
      if(x.contains("/")){
        x
      }else{
        Array("0")
      }
    }.distinct.diff(Array("0"))
    val lastFlash = f.diff(fs).map(_.mkString).diff(Array("[FLASH] Input files:","[FLASH] Output files:"))
    flashFile.delete()
    val logs = tri ++ lastFlash
    FileUtils.writeLines(new File(outPath, "log.txt"), logs.asJava)
  }

  def dealWithPara1(outPath: String, data: mutable.Buffer[String]): String = {
    val path = Utils.toolPath + "/datas"
    val in1 = outPath + "/raw.data_1.fastq"
    val in2 = outPath + "/raw.data_2.fastq"
    val tmpDir = outPath + "/tmp"
    val out1 = tmpDir + "/r1_paired_out.fastq"
    val unout1 = tmpDir + "/r1_unpaired_out.fastq"
    val out2 = tmpDir + "/r2_paired_out.fastq"
    val unout2 = tmpDir + "/r2_unpaired_out.fastq"
    var command = s"perl ${path}/trimmomatic.pl java -jar ${path}/Trimmomatic-0.32/trimmomatic-0.32.jar PE -threads 1 " +
      s"${data(1)} ${in1} ${in2} ${out1} ${unout1} ${out2} ${unout2} "
    if (data(2) == "yes") {
      val adapter = path + "/Trimmomatic-0.32/adapters/" + data(3)
      command += s"ILLUMINACLIP:${adapter}:${data(4)}:${data(5)}:${data(6)} "
    }
    if (data(7) == "yes") {
      command += s"SLIDINGWINDOW:${data(8)}:${data(9)} "
    }
    if (data(10) == "yes") {
      command += s"MINLEN:${data(11)} "
    }
    if (data(12) == "yes") {
      command += s"LEADING:${data(13)} "
    }
    if (data(14) == "yes") {
      command += s"TRAILING:${data(15)} "
    }
    if (data(16) == "yes") {
      command += s"CROP:${data(17)} "
    }
    if (data(18) == "yes") {
      command += s"HEADCROP:${data(19)} "
    }
    command
  }

  def dealWithFlash1(outPath: String, data: mutable.Buffer[String]): String = {
    val command = s"perl ${Utils.toolPath}/datas/flash.pl -in1 ${outPath}/tmp/r1_paired_out.fastq -in2 ${outPath}/tmp/r2_paired_out.fastq " +
      s"-o ${outPath}/out.extendedFrags.fastq -log ${outPath}/flash_log.txt -p ${data(20)} -m ${data(21)} -M ${data(22)} -x ${data(23)}"
    command
  }

  def dealWithPara(data: paraData, outPath: String): String = {
    val path = Utils.path

    val in1 = outPath + "/raw.data_1.fastq"
    val in2 = outPath + "/raw.data_2.fastq"
    val tmpDir = outPath + "/tmp"
    val out1 = tmpDir + "/r1_paired_out.fastq"
    val unout1 = tmpDir + "/r1_unpaired_out.fastq"
    val out2 = tmpDir + "/r2_paired_out.fastq"
    val unout2 = tmpDir + "/r2_unpaired_out.fastq"
    var command = s"perl ${path}/trimmomatic.pl java -jar ${path}/Trimmomatic-0.32/trimmomatic-0.32.jar PE -threads 1 " +
      s"${data.encondingType} ${in1} ${in2} ${out1} ${unout1} ${out2} ${unout2} "
    if (data.stepMethod == "yes") {
      val adapter = path + "/Trimmomatic-0.32/adapters/" + data.adapter.head
      command += s"ILLUMINACLIP:${adapter}:${data.seed_mismatches.head}:${data.palindrome_clip_threshold.head}:${data.simple_clip_threshold.head} "
    }
    if (data.trimMethod == "yes") {
      command += s"SLIDINGWINDOW:${data.window_size.head}:${data.required_quality.head} "
    }
    if (data.minlenMethod == "yes") {
      command += s"MINLEN:${data.minlen.head} "
    }
    if (data.leadingMethod == "yes") {
      command += s"LEADING:${data.leading.head} "
    }
    if (data.trailingMethod == "yes") {
      command += s"TRAILING:${data.trailing.head} "
    }
    if (data.cropMethod == "yes") {
      command += s"CROP:${data.crop.head} "
    }
    if (data.headcropMethod == "yes") {
      command += s"HEADCROP:${data.headcrop.head} "
    }
    command
  }

  def dealWithFlash(data: flashData, outPath: String, type2: String): String = {
    val path = Utils.path
    val command = s"perl ${path}/flash.pl -in1 ${outPath}/tmp/r1_paired_out.fastq -in2 ${outPath}/tmp/r2_paired_out.fastq " +
      s"-o ${outPath}/out.extendedFrags.fastq -log ${outPath}/flash_log.txt -p ${type2} -m ${data.m} -M ${data.M} -x ${data.x}"
    command
  }

  def dataPage(proname: String, sample: String) = Action { implicit request =>
    val id = getUserIdAndProId(request.session, proname)
    val allName = Await.result(projectdao.getAllProject(id._1), Duration.Inf)
    val samples = Await.result(sampledao.getAllSample(id._1, id._2), Duration.Inf)

    Ok(views.html.fileupload.data(samples, allName, proname, sample))
  }

  def isRunCmd(sample: String, proname: String): Action[AnyContent] = Action.async { implicit request =>
    val account = request.session.get("user").get
    admindao.getIdByAccount(account).flatMap { userId =>
      projectdao.getIdByProjectname(userId, proname).map { proId =>
        var valid = "false"
        if (sample.length != 0) {
          sampledao.getByPosition(userId, proId, sample).map { x =>
            if (x.state == 0) {
              runCmd1(x.id, proname, request.session)
              valid = "true"
            }
          }
        }
        Ok(Json.obj("valid" -> valid))
      }
    }
  }

  def toDate(proname: String, sample: String) = Action { implicit request =>
    Redirect(routes.SampleController.dataPage(proname, sample))
  }

  case class updateSampleData(sampleId: Int, newsample: String)

  val updateSampleForm = Form(
    mapping(
      "sampleId" -> number,
      "newsample" -> text
    )(updateSampleData.apply)(updateSampleData.unapply)
  )

  def updateSample: Action[AnyContent] = Action { implicit request =>
    val data = updateSampleForm.bindFromRequest.get
    val newsample = data.newsample
    val sampleId = data.sampleId
    Await.result(sampledao.updateSample(sampleId, newsample), Duration.Inf)
    val json = Json.obj("valid" -> "true")
    Ok(Json.toJson(json))
  }

  def deleteSample(id: Int): Action[AnyContent] = Action { implicit request =>
    val ses = Await.result(sampledao.getAllById(id), Duration.Inf)
    Await.result(sampledao.deleteSample(id), Duration.Inf)
    val path = Utils.outPath(ses.accountid, ses.projectid, id)
    FileUtils.deleteDirectory(new File(path))
    val count = Await.result(sampledao.getAllSample(ses.accountid, ses.projectid), Duration.Inf)
    Await.result(projectdao.updateCount(ses.projectid, count.size), Duration.Inf)
    val json = Json.obj("valid" -> "true")
    Ok(Json.toJson(json))
  }

  def deployGet(id: Int) = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val deploy = GetHtml.deploy(row.accountid, row.projectid, id, row.sample)
    Ok(Json.obj("html" -> deploy))
  }

  def download(id: Int, code: Int) = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid, row.projectid, id)
    val (file, name) = if (code == 1) {
      (new File(path, "raw.data_1.fastq"), row.sample + "_1.fastq")
    } else if (code == 2) {
      (new File(path, "raw.data_2.fastq"), row.sample + "_2.fastq")
    } else {
      (new File(path, "out_file.fasta"), row.sample + ".fasta")
    }
    Ok.sendFile(file).withHeaders(
      CACHE_CONTROL -> "max-age=3600",
      CONTENT_DISPOSITION -> ("attachment; filename=" + name),
      CONTENT_TYPE -> "application/x-download"
    )
  }

  def getUserIdAndProId(session: Session, proname: String): (Int, Int) = {
    val account = session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val proId = Await.result(projectdao.getIdByProjectname(userId, proname), Duration.Inf)
    (userId, proId)
  }

  def openLogFile(id: Int): Action[AnyContent] = Action { implicit request =>
    val row = Await.result(sampledao.getAllById(id), Duration.Inf)
    val path = Utils.outPath(row.accountid,row.projectid,id)
    val log = FileUtils.readLines(new File(path,"/log.txt")).asScala
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

  case class newsampleData(newsample: String)

  val newsampleForm = Form(
    mapping(
      "newsample" -> text
    )(newsampleData.apply)(newsampleData.unapply)
  )

  def checkNewsample(proname:String) = Action.async { implicit request =>
    val ses = getUserIdAndProId(request.session,proname)
    val data = newsampleForm.bindFromRequest.get
    val newsample = data.newsample
      sampledao.getByP(ses._1, ses._2, newsample).map { y =>
        val valid = if (y.size == 0) {
          "true"
        } else {
          "false"
        }
        Ok(Json.obj("valid" -> valid))
      }
  }

  case class sampleData(sample: String)

  val sampleForm = Form(
    mapping(
      "sample" -> text
    )(sampleData.apply)(sampleData.unapply)
  )

  def checkSample(proname: String) = Action.async { implicit request =>
    val ses = getUserIdAndProId(request.session, proname)
    val data = sampleForm.bindFromRequest.get
    val sample = data.sample
    sampledao.getByP(ses._1, ses._2, sample).map { y =>
      val valid = if (y.size == 0) {
        "true"
      } else {
        "false"
      }
      Ok(Json.obj("valid" -> valid))
    }
  }

  def checkRef(proname: String) = Action.async { implicit request =>
    val id = getUserIdAndProId(request.session, proname)
    sampledao.getAllSample(id._1, id._2).flatMap { x =>
      Thread.sleep(2000)
      sampledao.getAllSample(id._1, id._2).map { y =>
        val s = x.diff(y)
        println(s.size)
        val valid = if (s.size != 0) {
          "true"
        } else {
          "false"
        }
        Ok(Json.obj("valid" -> valid))
      }
    }
  }

  def getAllSample(proname: String) = Action { implicit request =>
    val json = dealWithSample(proname, request.session)
    Ok(Json.toJson(json))
  }

  def dealWithSample(proname: String, session: Session) = {
    val id = getUserIdAndProId(session, proname)
    val samples = Await.result(sampledao.getAllSample(id._1, id._2), Duration.Inf)
    val json = samples.sortBy(_.id).reverse.map { x =>
      val sample = x.sample
      val seqs = x.seqs
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
           |<a class="fastq" href="/project/download?id=${x.id}&code=1" title="原始数据"><b>${x.sample}</b><b>_1.fastq</b></a>,
           |<a class="fastq" href="/project/download?id=${x.id}&code=2" title="原始数据"><b>${x.sample}</b><b>_2.fastq</b></a>,
           |<a class="fastq" href="/project/download?id=${x.id}&code=3" title="拆分结果"><b>${x.sample}</b><b>.fasta</b></a>
           """.stripMargin
      } else {
        ""
      }
      val operation = if (x.state == 1) {
        s"""
           |  <button class="update" onclick="updateSample(this)" value="${x.sample}" id="${x.id}" title="修改样品名"><i class="fa fa-pencil"></i></button>
           |  <button class="update" onclick="restart(this)" value="${x.id}" title="重新运行"><i class="fa fa-repeat"></i></button>
           |  <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
           |  <button class="delete" onclick="openDelete(this)" value="${x.sample}" id="${x.id}" title="删除样品"><i class="fa fa-trash"></i></button>
           """.stripMargin
      } else if (x.state == 2) {
        s"""<button class="delete" onclick="openDelete(this)" value="${x.sample}" id="${x.id}" title="删除样品"><i class="fa fa-trash"></i></button>
           | <button class="update" onclick="openLog(this)" value="${x.id}" title="查看日志"><i class="fa fa-file-text"></i></button>
         """.stripMargin
      } else {
        ""
      }
      Json.obj("sample" -> sample, "seqs" -> seqs, "state" -> state, "createdate" -> date, "results" -> results, "operation" -> operation)
    }
    json

  }

  def getAllSampleName(proname:String) = Action.async{implicit request=>
    val ses = getUserIdAndProId(request.session,proname)
    sampledao.getAllSample(ses._1,ses._2).map{x=>
      val sample = x.map { y =>
       val validSample = if (y.state == 1) {
          y.sample
        } else {
          "0"
        }
        validSample
      }.distinct.diff(Array("0")).sorted
      Ok(Json.toJson(sample))
    }
  }
}
