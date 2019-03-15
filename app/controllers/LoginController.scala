package controllers

import java.io.File
import javax.inject.Inject

import dao._
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import utils.{ExecCommand, Utils}

import scala.collection.JavaConverters._
import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration



class LoginController @Inject()(admindao: adminDao,projectdao:projectDao,sampledao:sampleDao) extends Controller {

  def admin = Action {
    Ok(views.html.adminAndLogin.admin())
  }

  case class userData(account: String, password: String)

  val userForm = Form(
    mapping(
      "account" -> text,
      "password" -> text
    )(userData.apply)(userData.unapply)
  )

  def login = Action.async { implicit request =>
    val data = userForm.bindFromRequest.get
    val account = data.account
    val password = data.password
    admindao.selectByName(account, password).map { x =>
        val (valid, message) = if(x.isDefined){("true","")}else{("false", "用户名或密码错误")}
        val json = Json.obj("valid" -> valid, "message" -> message)
        Ok(Json.toJson(json))
    }
  }

  def toIndex(account:String) : Action[AnyContent]=Action{ implicit request=>
        Redirect(routes.SampleController.home()).withSession(request.session + ("user" -> "March") +("id" -> "2"))
  }


  def logout = Action {
    Redirect(routes.LoginController.admin()).withNewSession
  }

  def sign = Action {
    Ok(views.html.adminAndLogin.login())
  }

  def signsuccess(account: String, password: String): Action[AnyContent] = Action { implicit request =>
    val row =(account,password)
    Await.result(admindao.addAccount(Seq(row)),Duration.Inf)
    val id = Await.result(admindao.getIdByAccount(account),Duration.Inf)
   new File(Utils.path + "/" + id).mkdirs()
    Ok(views.html.adminAndLogin.signSuccess())
  }

  def toSuccess = Action {
    Ok(views.html.adminAndLogin.signSuccess())
  }

  case class accountData(account: String)

  val accountForm = Form(
    mapping(
      "account" -> text
    )(accountData.apply)(accountData.unapply)
  )

  def checkAccount = Action.async { implicit request =>
    val data = accountForm.bindFromRequest.get
    val account = data.account
    admindao.selectName(account).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      val message = "用户名已存在！"
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  def selected = Action{implicit request=>
    val route = request.headers.toMap.filter(_._1 == "Referer").map(_._2).head.head
    val last = route.split("/").last
    val id =if(last.contains("proname")){
      last.split("=").last
    }else{last}
    Ok(Json.toJson(id))
  }

  def getDisk = Action { implicit request =>
    val sh = s"sh ${Utils.allpath}/getDisk.sh"
    val command = new ExecCommand
    command.exec(sh)
    val buffer = FileUtils.readLines(new File(s"${Utils.allpath}/disk.txt")).asScala
    val head = buffer.head.split(" ")
    val all = head.head
    val alls = all.split("T").head.toDouble
    val use = head.last
    val uses = use.split("T").head.split("G").head.toDouble
    val per = if (uses > 20) {
      uses / alls / 1024 * 100
    } else {
      uses / alls * 100
    }
    val json = Json.obj("all" -> all, "use" -> use, "per" -> per.formatted("%.2f"))
    Ok(Json.toJson(json))
  }



}
