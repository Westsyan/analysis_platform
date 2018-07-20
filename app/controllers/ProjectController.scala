package controllers

import java.io.File
import javax.inject.Inject

import dao._
import models.Tables.ProjectRow
import org.apache.commons.io.FileUtils
import play.api.data.Form
import play.api.data.Forms._
import play.api.libs.Jsonp
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, Controller}
import utils.Utils

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

class ProjectController @Inject()(admindao: adminDao, projectdao: projectDao, sampledao: sampleDao,otudao:otuDao) extends Controller {

  case class projectData(projectname: String, description: String)

  val projectForm = Form(
    mapping(
      "projectname" -> text,
      "description" -> text
    )(projectData.apply)(projectData.unapply)
  )

  def addProject: Action[AnyContent] = Action { implicit request =>
    val data = projectForm.bindFromRequest.get
    val projectname = data.projectname
    val description = data.description
    val accId = request.session.get("id").head.toInt
    val date = Utils.date
    val project = ProjectRow(0, accId, projectname, description, date, 0)
    Await.result(projectdao.addProject(Seq(project)), Duration.Inf)
    val proId = Await.result(projectdao.getIdByProjectname(accId, projectname), Duration.Inf)
    val path = Utils.path + "/" + accId + "/" + proId
    new File(path + "/data").mkdirs()
    new File(path + "/otu").mkdirs()
    Ok(Json.obj("valid" -> "true"))
  }

  case class projectnameData(projectname: String)

  val projectnameForm = Form(
    mapping(
      "projectname" -> text
    )(projectnameData.apply)(projectnameData.unapply)
  )

  def checkProjectname = Action.async { implicit request =>
    val data = projectnameForm.bindFromRequest.get
    val projectname = data.projectname
    val id = request.session.get("id").head.toInt
    projectdao.getProjectname(id, projectname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      val message = "项目已存在！"
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }

  case class newpronameData(proname: String)

  val newproForm = Form(
    mapping(
      "proname" -> text
    )(newpronameData.apply)(newpronameData.unapply)
  )

  def checkNewproname = Action.async { implicit request =>
    val data = newproForm.bindFromRequest.get
    val projectname = data.proname
    val id = request.session.get("id").head.toInt
    projectdao.getProjectname(id, projectname).map { x =>
      val valid = if (x.size == 0) {
        "true"
      } else {
        "false"
      }
      val message = "项目已存在！"
      val json = Json.obj("valid" -> valid, "message" -> message)
      Ok(Json.toJson(json))
    }
  }


  def deleteProject(id: Int): Action[AnyContent] = Action { implicit request =>
    val userId =  request.session.get("id").head.toInt
    Await.result(projectdao.deleteByProname(id), Duration.Inf)
    Await.result(sampledao.deleteByProId(userId, id), Duration.Inf)
    FileUtils.deleteDirectory(new File(Utils.path + "/" + userId + "/" + id))
    Ok(Json.obj("valid" -> "true"))
  }

  case class updatePronameData(proId: Int, proname: String,description1:String)

  val updatePronameForm = Form(
    mapping(
      "proId" -> number,
      "proname" -> text,
      "description1" -> text
    )(updatePronameData.apply)(updatePronameData.unapply)
  )

  def updateProname = Action.async { implicit request =>
    val data = updatePronameForm.bindFromRequest.get
    val id = data.proId
    val proname = data.proname
    val des = data.description1
    projectdao.getById(id).flatMap{p=>
      if(p.projectname == proname){
        projectdao.updateDescriptionById(id,des).map { x =>
          Ok(Json.obj("valid" -> "true"))
        }
        }else{
          projectdao.updatePronameById(id, proname).flatMap { x =>
            projectdao.updateDescriptionById(id, des).map { x =>
              Ok(Json.obj("valid" -> "true"))
            }
          }
        }
      }
  }

  case class updateDescriptionData(proId2: Int, newdescription: String)

  val updateDescriptionForm = Form(
    mapping(
      "proId2" -> number,
      "newdescription" -> text
    )(updateDescriptionData.apply)(updateDescriptionData.unapply)
  )

  def updateDescription = Action.async { implicit request =>
    val data = updateDescriptionForm.bindFromRequest.get
    val id = data.proId2
    val des = data.newdescription
    projectdao.updateDescriptionById(id, des).map { x =>
      Ok(Json.obj("valid" -> "true"))
    }
  }

  def deleteAll(id:Int)= Action{implicit request=>
    val user = request.session.get("admin").getOrElse("user")
    val userid = request.session.get("id").getOrElse("0")
    if(user == "admin" && userid == "1") {
      val run = Future {
        Await.result(otudao.deleteByUserid(id), Duration.Inf)
        Await.result(sampledao.deleteByUserid(id), Duration.Inf)
        Await.result(projectdao.deleteByUserid(id), Duration.Inf)
      }
    }
    val json = Json.toJson("success")
    request.queryString.get("callback").flatMap(_.headOption) match {
      case Some(callback) => Ok(Jsonp(callback, json))
      case None => Ok(json)
    }
  }

}
