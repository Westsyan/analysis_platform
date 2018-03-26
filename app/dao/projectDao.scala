package dao

import javax.inject.Inject


import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class projectDao  @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def addProject(project: Seq[(String,String,String,String,Int)]) : Future[Unit] = {
    db.run(Project.map(x=>(x.account,x.projectname,x.description,x.createdata,x.samcount)) ++= project).map(x=>())
  }

  def getProjectname(account:String,projectname:String) : Future[Seq[ProjectRow]] = {
    db.run(Project.filter(_.account === account).filter(_.projectname === projectname).result)
  }

  def getAllProject(account:String) : Future[Seq[String]] ={
    db.run(Project.filter(_.account === account).map(_.projectname).result)
  }

  def getAll(account : String) : Future[Seq[ProjectRow]] = {
    db.run(Project.filter(_.account === account).result)
  }

  def getIdByProjectname(account:String,projectname:String) : Future[Int] ={
    db.run(Project.filter(_.account === account).filter(_.projectname === projectname).map(_.id).result.head)
  }

}
