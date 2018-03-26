package utils

import java.io.File
import java.text.SimpleDateFormat
import java.util.Date

import org.apache.commons.io.FileUtils
import play.api.mvc.{AnyContent, Request}

/**
  * Created by yz on 2017/6/16.
  */
object Utils {
  
  def getTime(startTime: Long) = {
    val endTime = System.currentTimeMillis()
    (endTime - startTime) / 1000.0
  }

  def deleteDirectory(direcotry: File) = {
    try {
      FileUtils.deleteDirectory(direcotry)
    } catch {
      case _ =>
    }
  }

  def deleteDirectory(tmpDir: String):Unit = {
    val direcotry = new File(tmpDir)
    deleteDirectory(direcotry)
  }

  def isDouble(value: String): Boolean = {
    try {
      value.toDouble
    } catch {
      case _: Exception =>
        return false
    }
    true
  }

  def refer(request:Request[AnyContent]):String = {
    val header = request.headers.toMap
    header.filter(_._1 == "Referer").map(_._2).head.head
  }

  def date : String = {
    val now = new Date()
    val dateFormat = new SimpleDateFormat("yyyy-MM-dd")
    val date = dateFormat.format(now)
    return date
  }




  val windowsPath = "F:/analysis_data"
  val linuxPath = "/root/projects/analysis_data/"
  val path = {
    if (new File(windowsPath).exists()) windowsPath else linuxPath
  }
}
