package wordextractor.foregroundobjectdirection

import io.Frame
import wordextractor.Word
import wordextractor.WordExtractor
import org.opencv.core.Core
import org.opencv.core.Mat
import org.opencv.core.MatOfPoint
import org.opencv.core.MatOfPoint2f
import org.opencv.core.Scalar
import org.opencv.imgproc.Imgproc
import org.opencv.video.BackgroundSubtractorMOG2
import util.Util.bucket

import scala.collection.immutable
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._

class ForegroundObjectDirectionExtractor(configuration: ForegroundObjectDirectionConfiguration)
  extends WordExtractor[Mat] {

  val backgroundSubtractor = new BackgroundSubtractorMOG2()
  var processedFrameCount = 0

  override def getWords(frame: Frame): Option[(immutable.Seq[Word], Mat)] = {
    val foreground = new Mat
    backgroundSubtractor(frame.image, foreground, configuration.backgroundSubtractorLearningRate)
    processedFrameCount += 1

    // Give the background subtractor some time to learn the background.
    if (processedFrameCount > 20) {
      Some(getWordsFromForeground(frame.copy(image = foreground)))
    } else {
      None
    }
  }

  private[this] def getWordsFromForeground(foreground: Frame): (immutable.Seq[Word], Mat) = {
    // Initialize a visualization image.
    val visualization = new Mat
    foreground.image.copyTo(visualization)
    Imgproc.cvtColor(visualization, visualization, Imgproc.COLOR_GRAY2BGR)

    // Remove the shadow pixels.
    Imgproc.threshold(foreground.image, foreground.image, 200, 255, Imgproc.THRESH_BINARY)

    // Make it less noisy.
    Imgproc.erode(foreground.image, foreground.image, new Mat)
    Imgproc.dilate(foreground.image, foreground.image, new Mat)

    // Find contours.
    val contours = ListBuffer[MatOfPoint]()
    Imgproc.findContours(
      foreground.image, contours, new Mat, Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_NONE)

    // Fit ellipses to the contours.
    val ellipses = contours.flatMap { contour =>
      if (contour.toList.size >= 5) {
        val matofpoint2f = new MatOfPoint2f()
        matofpoint2f.fromList(contour.toList)
        Some(Imgproc.fitEllipse(matofpoint2f))
      } else {
        None
      }
    }

    // Turn the ellipses into words.
    val words = ellipses.flatMap { ellipse =>
      // TODO: Make this threshold configurable?
      if (ellipse.size.area > 1000.0) {
        val minAngle = 90.0
        val maxAngle = 270.0
        val angle = {
          var angle = ellipse.angle
          while (angle < minAngle) angle += 180.0
          while (angle > maxAngle) angle -= 180.0
          angle
        }

        Some(ForegroundObjectDirection(
          configuration,
          bucket(angle, minAngle, maxAngle, configuration.directionBucketCount),
          bucket(ellipse.center.x, 0.0, foreground.image.size.width, configuration.xBucketCount),
          bucket(ellipse.center.y, 0.0, foreground.image.size.height, configuration.yBucketCount)))
      } else {
        None
      }
    }

    // Create a beautiful visualization.
    Imgproc.drawContours(visualization, contours, -1, new Scalar(0, 255, 0), 1)
    ellipses.foreach { ellipse =>
      Core.ellipse(visualization, ellipse, new Scalar(0, 0, 255))
      Core.circle(visualization, ellipse.center, 2, new Scalar(0, 0, 255), 1)
    }

    (words.toList, visualization)
  }

}