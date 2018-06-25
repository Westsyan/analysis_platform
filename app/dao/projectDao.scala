package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class projectDao  @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def addProject(project: Seq[ProjectRow]) : Future[Unit] = {
    db.run(Project ++= project).map(_=>())
  }

  def getProjectname(accountid:Int,projectname:String) : Future[Seq[ProjectRow]] = {
    db.run(Project.filter(_.accountid === accountid).filter(_.projectname === projectname).result)
  }

  def getAllProject(accountid:Int) : Future[Seq[String]] ={
    db.run(Project.filter(_.accountid === accountid).map(_.projectname).result)
  }

  def getAll(accountid : Int) : Future[Seq[ProjectRow]] = {
    db.run(Project.filter(_.accountid === accountid).result)
  }

  def getIdByProjectname(accountid:Int,projectname:String) : Future[Int] ={
    db.run(Project.filter(_.accountid === accountid).filter(_.projectname === projectname).map(_.id).result.head)
  }

  def updateCount(id:Int,row:Int) : Future[Unit] = {
    db.run(Project.filter(_.id === id).map(_.samcount).update(row).map(_ => ()))
  }

  def getProject(accountid:Int,projectname:String) : Future[ProjectRow] = {
    db.run(Project.filter(_.accountid === accountid).filter(_.projectname === projectname).result.head)
  }

  def deleteByProname(proId:Int) :Future[Unit] ={
    db.run(Project.filter(_.id === proId).delete).map(_=>())
  }

  def deleteByUserid(id:Int) : Future[Unit] = {
    db.run(Project.filter(_.accountid === id).delete).map(_ => ())
  }

  def updatePronameById(id:Int,proname:String) : Future[Unit] = {
    db.run(Project.filter(_.id === id).map(_.projectname).update(proname)).map(_=>())
  }

  def updateDescriptionById(id:Int,description:String) : Future[Unit] = {
    db.run(Project.filter(_.id === id).map(_.description).update(description)).map(_=>())
  }

  def getById(id:Int) : Future[ProjectRow] = {
    db.run(Project.filter(_.id === id).result.head)
  }
}
