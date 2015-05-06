package app

import java.io.File
import java.net.URL

import com.atul.JavaOpenCV.Imshow
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.core.MatOfPoint
import org.opencv.core.MatOfPoint2f
import org.opencv.core.Point
import org.opencv.core.Scalar
import org.opencv.highgui.Highgui
import org.opencv.imgproc.Imgproc
import org.opencv.video.BackgroundSubtractorMOG2
import scala.collection.JavaConversions._

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import sys.process._

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.math._

class Foreground(name: String, url: String) {
  def recursiveListFiles(f: File): immutable.Seq[File] = {
    val these = f.listFiles.toList
    these.filterNot(_.isDirectory) ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  val boundaries = List(
    (new Point(103, 20), new Point(78, 240)),
    (new Point(128, 20), new Point(352, 200)),
    (new Point(146, 20), new Point(352, 113)),
    (new Point(94, 97), new Point(295, 87)))

  val greenDistribution = DirectionMeasurementDistribution(130, 200, 20, 0.1)
  val redDistribution = DirectionMeasurementDistribution(220, 110, 20, 0.1)

  def drawBoundaries(mat: Mat): Unit = {
    boundaries.foreach { case (a, b) =>
      Core.line(mat, a, b, new Scalar(255, 0, 0))
    }
  }

  def mkdirp(path: String): Unit = {
    var prepath = ""
    for (dir <- path.split("/")) {
      prepath += (dir + "/")
      new File(prepath).mkdir()
    }
  }

  def run(): Unit = {
    val imshow = new Imshow("Hello World!")

    var lightProbability = LightProbability(0.5, 0.5)

    val back = new Mat
    val fore = new Mat
    val subtractor = new BackgroundSubtractorMOG2()

    /*val files = recursiveListFiles(new File(s"frames/$name")).map(_.getPath).filter { path =>
      path.matches(".*\\.jpeg$")
    }*/

    /*val example =  Highgui.imread(files(0))
    val average = new Mat(example.size, example.`type`, new Scalar(0, 0, 0))

    files.foreach { path =>
      val frame = Highgui.imread(path)
      println(average.size)
      println(frame.size)
      Core.addWeighted(average, 1.0f, frame, 1.0f/files.size, 0.0f, average)
    }

    Highgui.imwrite("average.png", average)

    (0 until 50).foreach { _ =>
      subtractor(average, fore)
    }*/

    (0 until 10000).foreach { case index =>
      val now = DateTime.now
      val dateString = DateTimeFormat.forPattern("yyyyMMdd").print(now)
      val timeString = DateTimeFormat.forPattern("HHmmss").print(now)
      mkdirp(s"frames/$name/$dateString")
      new URL(url) #> new File(s"frames/$name/$dateString/$timeString.jpeg") !!;
      val frame = Highgui.imread(s"frames/$name/$dateString/$timeString.jpeg")

    /*files.zipWithIndex.foreach { case (path, index) =>
      val frame = Highgui.imread(path)*/
      subtractor(frame, fore, 0.01)
      if (index > 10) {
        val fin = new Mat
        val forecolor = new Mat
        Imgproc.erode(fore, fore, new Mat)
        Imgproc.dilate(fore, fore, new Mat)

        val contours = ListBuffer[MatOfPoint]()
        Imgproc.findContours(fore, contours, new Mat, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE)

        val ellipses = contours.flatMap { contour =>
          if (contour.toList.size >= 5) {
            val matofpoint2f = new MatOfPoint2f()
            matofpoint2f.fromList(contour.toList)
            Some(Imgproc.fitEllipse(matofpoint2f))
          } else {
            None
          }
        }

        Imgproc.cvtColor(fore, forecolor, Imgproc.COLOR_GRAY2BGR)

        Imgproc.drawContours(forecolor, contours, -1, new Scalar(0, 255, 0), 2)
        ellipses.foreach { ellipse =>
          if (ellipse.size.area > 1000.0) {
            Core.ellipse(forecolor, ellipse, new Scalar(0, 0, 255))
            Core.circle(forecolor, ellipse.center, 4, new Scalar(0, 0, 255), 4)
            //Core.putText(forecolor, "%.1f".format(ellipse.size.area), ellipse.center, Core.FONT_HERSHEY_PLAIN, 1.0, new Scalar(255, 255, 255))

            var angle = ellipse.angle
            while (angle < 90.0) angle += 180.0
            while (angle > 270.0) angle -= 180.0

            val greenUpdate = greenDistribution.density(angle)
            val redUpdate = redDistribution.density(angle)

            Core.putText(forecolor, "%.1f %.1f".format(greenUpdate, redUpdate), ellipse.center, Core.FONT_HERSHEY_PLAIN, 1.0, new Scalar(255, 255, 255))

            lightProbability = lightProbability.update(greenUpdate, redUpdate)
          }
        }
        lightProbability = lightProbability.stepTime(60)

        Core.putText(forecolor, "%.2f %.2f".format(lightProbability.green, lightProbability.red), new Point(10, 40), Core.FONT_HERSHEY_PLAIN, 2.0, new Scalar(255, 255, 255))

        drawBoundaries(forecolor)
        drawBoundaries(frame)

        Core.hconcat(List(frame, forecolor), fin)
        //Highgui.imwrite(s"$path.fore.png", fin)

        imshow.showImage(fin)

        Thread.sleep(1000)
      }
    }
  }
}
