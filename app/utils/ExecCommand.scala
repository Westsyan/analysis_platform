package utils

import java.io.File

import scala.sys.process._

class ExecCommand {
  var isSuccess: Boolean = false
  val err = new StringBuilder
  val out = new StringBuilder
  val log = ProcessLogger(out append _, err append _)

  def exec(command: String) = {
    val exitCode = command ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1: String, command2: String) = {
    val exitCode = (command1 #&& command2) ! log
    if (exitCode == 0) isSuccess = true
  }

  def execs(command1: String, command2: String, command3:String, command4 :String) = {
    val exitCode = (command1 #&& command2 #&& command3 #&& command4) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command: String, outFile: File) = {
    val exitCode = (command #> outFile) ! log
    if (exitCode == 0) isSuccess = true
  }

  def exec(command1:String,command2:String,tmpDir:String) = {
    val exitCode1 = Process(command1 , new File(tmpDir)) ! log
    val exitCode2 = Process(command2 , new File(tmpDir)) ! log
    if(exitCode1 == 0 && exitCode2 == 0) isSuccess = true
  }

  def exe(command:String, tmpDir:String) = {
    val exitCode = Process(command,new File(tmpDir)) ! log
    if (exitCode == 0) isSuccess = true
  }

  def getErrStr = {
    err.toString()
  }

  def getOutStr = {
    out.toString()
  }


}
