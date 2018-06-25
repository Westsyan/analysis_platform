package test

import java.io.File

object test {

    def main(args: Array[String]): Unit = {
      println((new File("D:/")).exists())
    }


}
