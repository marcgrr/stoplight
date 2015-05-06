package io

import java.io.File

import org.joda.time.DateTime
import org.opencv.highgui.Highgui

import scala.collection.immutable

class DirectoryFrameReader(path: String) extends FrameReader {
  var index = 0

  val files = recursiveListFiles(new File(path)).filter { file =>
    file.getPath.matches(".*\\.jpeg")
  }

  override def read(): (Int, Frame) = {
    val file = files(index)
    val capturedAt = new DateTime(file.lastModified)
    var image = Highgui.imread(file.getPath)
    val ret = (index, Frame(capturedAt, image))
    index += 1

    if (image.size.width == 0.0) {
      read()
    } else {
      ret
    }
  }

  private[this] def recursiveListFiles(f: File): immutable.Seq[File] = {
    val these = f.listFiles.toList
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }
}
