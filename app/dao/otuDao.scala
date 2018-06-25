package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class otuDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile]{

  import profile.api._

  def addOtuInfo(taskRow:Seq[OtuRow]) : Future[Unit] = {
    db.run(Otu ++= taskRow).map(_ =>())
  }

  def getAllByPosition(userId:Int,proId:Int,otuname:String) : Future[OtuRow] = {
    db.run(Otu.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.otuname === otuname).result.head)
  }

  def getAllByPosi(userId:Int,proId:Int,otuname:String) : Future[Seq[OtuRow]] = {
    db.run(Otu.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.otuname === otuname).result)
  }

  def updateState(id:Int,state:Int) : Future[Unit] = {
    db.run(Otu.filter(_.id === id).map(_.state).update(state)).map(_=>())
  }

  def getAllOtuByPosition(userId:Int,proId:Int) : Future[Seq[OtuRow]] = {
    db.run(Otu.filter(_.accountid === userId).filter(_.projectid === proId).result)
  }

  def deleteOtu(id:Int) : Future[Unit] = {
    db.run(Otu.filter(_.id === id).delete).map(_ =>())
  }

  def deleteByUserid(id:Int) : Future[Unit] = {
    db.run(Otu.filter(_.accountid === id).delete).map(_ => ())
  }

  def getById(id:Int) : Future[OtuRow] ={
    db.run(Otu.filter(_.id === id).result.head)
  }

  def updateOtuName(id:Int,newName:String) : Future[Unit] = {
    db.run(Otu.filter(_.id === id).map(_.otuname).update(newName)).map(_ =>())
  }

}
