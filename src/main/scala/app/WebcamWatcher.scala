package app

import java.io.File
import java.net.URL
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import sys.process._

class WebcamWatcher(name: String, url: String) {
  def mkdirp(path: String): Unit = {
    var prepath = ""
    for (dir <- path.split("/")) {
      prepath += (dir + "/")
      new File(prepath).mkdir()
    }
  }

  def watch(): Unit = {
    (0 until 600).foreach { _ =>
      val now = DateTime.now
      val dateString = DateTimeFormat.forPattern("yyyyMMdd").print(now)
      val timeString = DateTimeFormat.forPattern("HHmmss").print(now)
      mkdirp(s"frames/$name/$dateString")
      new URL(url) #> new File(s"frames/$name/$dateString/$timeString.jpeg") !!;
      Thread.sleep(1000)
    }
  }
}