package controllers

import java.io.File
import javax.inject.Inject

import dao._
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.json.Json
import play.api.mvc._
import utils.Utils

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
        Redirect(routes.SampleController.home()).withSession(request.session + ("user" -> account))
  }

  def index = Action{ implicit request =>
    val account = request.session.get("user").head
    val id = Await.result(admindao.getIdByAccount(account),Duration.Inf)
    val x = Await.result(projectdao.getAllProject(id),Duration.Inf)
    Ok(views.html.background.index(x))
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





}
