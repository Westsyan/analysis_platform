package test

import java.text.SimpleDateFormat
import java.util.Date

object test {

    def main(args: Array[String]): Unit = {

    val now  = new Date()
      val dateFormat = new SimpleDateFormat("yyMMddHHmmss")
      val date = dateFormat.format(now)
      println(date + "_OTU")

    }

}
