package wordextractor.channelintensity

import io.Frame
import wordextractor.Word
import wordextractor.WordExtractor
import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.core.Scalar
import org.opencv.imgproc.Imgproc
import util.Util.bucket

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

class ChannelIntensityExtractor(configuration: ChannelIntensityConfiguration)
  extends WordExtractor[Mat] {

  var frameHistory = ListBuffer[Frame]()

  def getWords(frame: Frame): Option[(immutable.Seq[Word], Mat)] = {
    frameHistory.append(frame)
    if (frameHistory.size > 20) {
      frameHistory.remove(0)
    }
    if (frameHistory.size == 20) {
      Some(gogogo())
    } else {
      None
    }

  }

  def gogogo(): (immutable.Seq[Word], Mat) = {
    val first = frameHistory.take(10).toList.map(_.image)
    val last = frameHistory.takeRight(10).toList.map(_.image)

    val firstAverage = average(first)
    val lastAverage = average(last)

    val oneWay = new Mat
    val otherWay = new Mat
    firstAverage.copyTo(oneWay)
    firstAverage.copyTo(otherWay)

    Core.subtract(lastAverage, firstAverage, oneWay)
    Core.subtract(firstAverage, lastAverage, otherWay)
    Core.hconcat(List(oneWay, otherWay), oneWay)

    (List.empty, oneWay)
  }

  private[this] def average(mats: immutable.Seq[Mat]): Mat = {
    println(mats.map(_.size))
    val dest = new Mat
    mats(0).copyTo(dest)
    dest.setTo(new Scalar(0, 0, 0))
    mats.foreach { mat =>
      Core.addWeighted(mat, 1.0 / mats.size.toDouble, dest, 1.0, 0.0, dest)
    }
    dest
  }

  def getWordsBla(frame: Frame): Option[(immutable.Seq[Word], Mat)] = {
    // Initialize a visualization image.
    val visualization = new Mat
    frame.image.copyTo(visualization)

    // Extract the channel.
    val channels = {
      val channelBuffer = new ListBuffer[Mat]()
      Core.split(frame.image, channelBuffer)
      if (channelBuffer.size < 3) return None
      channelBuffer
    }

    val channel = channels(configuration.channelIndex)

    val brightness = new Mat
    Imgproc.cvtColor(frame.image, brightness, Imgproc.COLOR_BGR2GRAY)

    val xSize = channel.cols
    val xBucketSize = xSize / configuration.xBucketCount

    val ySize = channel.rows
    val yBucketSize = ySize / configuration.yBucketCount

    val words = (0 until configuration.xBucketCount).flatMap { xBucket =>
      (0 until configuration.yBucketCount).map { yBucket =>
        // Construct the word.
        val xRange = new org.opencv.core.Range(
          xBucketSize * xBucket,
          xBucketSize * (xBucket + 1) - 1)
        val yRange = new org.opencv.core.Range(
          yBucketSize * yBucket,
          yBucketSize * (yBucket + 1) - 1)
        val intensity = Core.mean(channel.submat(yRange, xRange)).`val`(0)
        val oi = Core.mean(brightness.submat(yRange, xRange)).`val`(0)
        val word = ChannelIntensity(
          configuration,
          bucket(intensity / oi, 0.0, 1.0, configuration.intensityBucketCount),
          xBucket,
          yBucket)

        println(intensity)
        println(oi)

        // Visualize the word on the visualization.
        val colorfulIntensity = new Scalar(
          (0 until 3).map { channelIndex =>
            if (channelIndex == configuration.channelIndex) {
              256.0 * intensity / oi
            } else {
              0.0
            }
          }.toArray)
        visualization.submat(yRange, xRange).setTo(colorfulIntensity)

        word
      }
    }

    /*val toproc = new Mat
    frame.image.copyTo(toproc)
    Imgproc.cvtColor(toproc, toproc, Imgproc.COLOR_BGR2GRAY)*/
    //val toproc = channel

    //Imgproc.adaptiveThreshold(toproc, visualization, 255.0, Imgproc.ADAPTIVE_THRESH_GAUSSIAN_C, Imgproc.THRESH_BINARY, 101, 0)
    /*Imgproc.cvtColor(visualization, visualization, Imgproc.COLOR_BGR2GRAY)
    Imgproc.threshold(channel, visualization, 200.0, 255.0, Imgproc.THRESH_BINARY)
    Imgproc.cvtColor(visualization, visualization, Imgproc.COLOR_GRAY2BGR)*/

    val r1 = new Mat
    val r2 = new Mat

    val hsv = new Mat
    frame.image.copyTo(hsv)
    Imgproc.cvtColor(hsv, hsv, Imgproc.COLOR_BGR2HSV)
    Core.inRange(hsv, new Scalar(0, 100, 10), new Scalar(5, 256, 256), r1)
    Core.inRange(hsv, new Scalar(250, 100, 10), new Scalar(256, 256, 256), r2)
    Core.bitwise_or(r1, r2, r1)
    Imgproc.cvtColor(r1, r1, Imgproc.COLOR_GRAY2BGR)

    Some((words, r1))
  }
}