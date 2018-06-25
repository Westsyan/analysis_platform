package dao

import javax.inject.Inject

import models.Tables._
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class sampleDao  @Inject()(protected val dbConfigProvider: DatabaseConfigProvider) extends
  HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  def addSample(samples: Seq[SampleRow]): Future[Unit] = {
    db.run(Sample ++= samples).map(_=>())
  }

  def getIdByPosition(accountid:Int,projectid:Int,sample:String) : Future[Int] = {
    db.run(Sample.filter(_.accountid === accountid).filter(_.projectid === projectid).filter(_.sample === sample).
      map(_.id).result.head)
  }

  def getByPosition(accountid:Int,projectid:Int,sample:String) : Future[SampleRow] = {
    db.run(Sample.filter(_.accountid === accountid).filter(_.projectid === projectid).filter(_.sample === sample).result.head)
  }

  def getByP(accountid:Int,projectid:Int,sample:String) : Future[Seq[SampleRow]] = {
    db.run(Sample.filter(_.accountid === accountid).filter(_.projectid === projectid).filter(_.sample === sample).result)
  }

  def getAllSample(accountid:Int,projectid:Int) : Future[Seq[SampleRow]] = {
    db.run(Sample.filter(_.accountid === accountid).filter(_.projectid === projectid).result)
  }

  def update(row:SampleRow) : Future[Unit] = {
    db.run(Sample.filter(_.id === row.id).update(row)).map(_=>())
  }

  def updateSample(id:Int,sample:String) : Future[Unit] = {
    db.run(Sample.filter(_.id === id).map(_.sample).update(sample)).map(_=>())
  }

  def deleteSample(id:Int) :Future[Unit] = {
    db.run(Sample.filter(_.id === id).delete).map(_=>())
  }

  def deleteByUserid(id:Int) : Future[Unit] = {
    db.run(Sample.filter(_.accountid === id).delete).map(_ => ())
  }

  def deleteByProId(accountId:Int,proId:Int) : Future[Unit] = {
    db.run(Sample.filter(_.accountid === accountId).filter(_.projectid === proId).delete).map(_=>())
  }

  def getAllById(id:Int) : Future[SampleRow] = {
    db.run(Sample.filter(_.id === id).result.head)
  }

  def checkByPosition(userId:Int,proId:Int,sample: String) : Future[Seq[SampleRow]] ={
    val samples = sample.split(",").map(_.trim).distinct
    db.run(Sample.filter(_.accountid === userId).filter(_.projectid === proId).filter(_.sample.inSetBind(samples)).result)
  }


}
