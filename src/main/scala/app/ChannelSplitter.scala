package app

import java.io.File

import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.highgui.Highgui

import scala.collection.immutable
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class ChannelSplitter(name: String) {
  def recursiveListFiles(f: File): immutable.Seq[File] = {
    val these = f.listFiles.toList
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  def run(): Unit = {
    val files = recursiveListFiles(new File(s"frames/$name")).map(_.getPath)
    files.foreach { path =>
      val image = Highgui.imread(path)
      val buffer = new ListBuffer[Mat]
      Core.split(image, buffer)

      println(buffer)

      buffer.zipWithIndex.map { case (channel, index) =>
        Highgui.imwrite(s"$path.$index.png", channel)
      }
    }
  }
}
