package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class adminDao @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def selectByName(account:String,password:String) : Future[Option[UserRow]] = {
    db.run(User.filter(_.account === account).filter(_.password === password).result.headOption)
  }

  def addAccount(account: Seq[(String,String)]) : Future[Unit] =
    db.run(User.map(x=>(x.account,x.password)) ++= account).map(_ => ())


  def selectName(account:String) : Future[Seq[UserRow]] = {
    db.run(User.filter(_.account === account).result)
  }

  def updatePassword(account:String,password:String) : Future[Unit] = {
    selectByName(account,password).map{x=>
      val row = UserRow(x.get.id,account,password)
      db.run(User.filter(_.account === account).update(row).map(_ => ()))
    }
  }

  def getIdByAccount(account:String) : Future[Int] ={
    db.run(User.filter(_.account === account).map(_.id).result.head)
  }



}
