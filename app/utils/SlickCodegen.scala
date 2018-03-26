package utils

import java.util.concurrent.TimeUnit

import slick.codegen.SourceCodeGenerator
import slick.jdbc.MySQLProfile

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}


/**
  * Created by yz on 2017/5/5.
  */
object SlickCodegen {
  def main(args: Array[String]): Unit = {
    val slickDriver = "slick.jdbc.MySQLProfile"
    val url = "jdbc:mysql://localhost:3306/analysis_platform?useUnicode=true&characterEncoding=UTF-8&useSSL=false"
    val user = "root"
    val password = "159875321"

    val db = MySQLProfile.api.Database.forURL(url, user, password)
    val dbio = MySQLProfile.createModel()
    val model = db.run(dbio)
    val future: Future[SourceCodeGenerator] = model.map(model => new SourceCodeGenerator(model) {
      override def code = "import com.github.tototoshi.slick.MySQLJodaSupport._\n" +
        "import org.joda.time.DateTime\n" + super.code
      override def Table = new Table(_) {
        override def Column = new Column(_) {
          override def rawType = model.tpe match {
            case "java.sql.Timestamp" => "DateTime" // kill j.s.Timestamp
            case _ =>
              super.rawType
          }
        }
      }
    })
    val codegen: SourceCodeGenerator = Await.result(future, Duration.create(5, TimeUnit.MINUTES))
    codegen.writeToFile(slickDriver, "app", "models", "Tables", "Tables.scala")
  }


}
