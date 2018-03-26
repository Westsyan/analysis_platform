package controllers

import java.io.File
import javax.inject.Inject

import dao.{adminDao, projectDao}
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import utils.{ExecCommand, Utils, fastqToFasta}

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class UploadController @Inject()(admindao: adminDao, projectdao: projectDao) extends Controller {

  def enterHome(projectname: String): Action[AnyContent] = Action { implicit request =>
    val account = request.session.get("user").head
    val allName = Await.result(projectdao.getAllProject(account), Duration.Inf)
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val projectId = Await.result(projectdao.getIdByProjectname(account, projectname), Duration.Inf)
    val data = new File(Utils.path + "/" + userId + "/" + projectId + "/data")
    if (data.length() < 2) {
      Ok(views.html.fileupload.uploadFile(allName, projectname))
    } else {
      Ok(Json.toJson(""))
    }
  }

  def home = Action { implicit request =>
    val account = request.session.get("user").head
    val all = Await.result(projectdao.getAll(account),Duration.Inf)
    val projectname = all.map(_.projectname)
    Ok(views.html.background.home(all,projectname))
  }

  case class paraData(sample: String, encondingType: String, stepMethod: String, adapter: Option[String],
                      seed_mismatches: Option[Int], palindrome_clip_threshold: Option[Int],
                      simple_clip_threshold: Option[Int], trimMethod: String, window_size: Option[Int],
                      required_quality: Option[Int], minlenMethod: String, minlen: Option[Int],
                      leadingMethod: String, leading: Option[Int], trailingMethod: String, trailing: Option[Int],
                      cropMethod: String, crop: Option[Int], headcropMethod: String, headcrop: Option[Int])


  val paraForm = Form(
    mapping(
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
      "headCrop" -> optional(number)
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

  def uploadFile(proname:String) = Action(parse.multipartFormData) { implicit request =>
    val path = Utils.path
    val account = request.session.get("user").head
    val userId = Await.result(admindao.getIdByAccount(account), Duration.Inf)
    val projectId = Await.result(projectdao.getIdByProjectname(account, proname), Duration.Inf)

    val file = request.body.files
    val paradata = paraForm.bindFromRequest.get
    val flashdata = flashForm.bindFromRequest.get
    val type1 = paradata.encondingType
    val type2 = type1.split("-phred").mkString
    val sample = paradata.sample
    val in1 = file(0).ref.file.getPath
    val in2 = file(1).ref.file.getPath
    val outPath =  path + "/" + userId + "/" + projectId + "/" + sample
    FileUtils.copyFile(new File(in1),new File(outPath + "/raw.split_1.fastq"))
    FileUtils.copyFile(new File(in2),new File(outPath + "/raw.split_2.fastq"))
    new File(outPath + "/tmp").mkdir()
    val command1 = dealWithPara(paradata,outPath)
    val command2 = dealWithFlash(flashdata,outPath,type2)
    println(command2)
    val command = new ExecCommand
    command.exec(command1,command2)
    if(command.isSuccess){
      val fastq = outPath + "/tmpDir/out.extendedFrags.fastq"
      val fasta = outPath + "/out_file.fasta"
      fastqToFasta.fqToFa(fastq,fasta)
      Ok(Json.obj("valid" -> "true"))
    }else{
      Ok(Json.obj("valid" -> "flase","message" -> command.getErrStr))
    }

  }

  def dealWithPara(data: paraData, outPath: String) = {
    val path = Utils.path
    val sample = data.sample
    val in1 = outPath + "/raw.split_1.fastq"
    val in2 = outPath + "/raw.split_2.fastq"
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

  def dealWithFlash(data:flashData,outPath:String,type2:String) ={
    val path = Utils.path
    val command =if(new File("C:/").exists()){
      s"perl ${path}/flash.pl -in1${outPath}/tmp/r1_paired_out.fastq ${outPath}/tmp/r2_paired_out.fastq " +
      s"-o ${outPath}/out.extendedFrags.fastq -log ${outPath}/flash_log.txt -html ${outPath}/tmpDir/output.html " +
      s" -p ${type2} -m ${data.m} -M ${data.M} -x ${data.x}"
    }else {
      s"flash ${outPath}/tmp/r1_paired_out.fastq ${outPath}/tmp/r2_paired_out.fastq " +
        s"-m ${data.m} -M ${data.M} -x ${data.x} -p ${type2} -d ${outPath}/tmpDir"
    }
      command
  }
}
