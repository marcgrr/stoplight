package app

import org.opencv.core.Core
import org.opencv.core.MatOfRect
import org.opencv.core.Point
import org.opencv.core.Scalar
import org.opencv.highgui.Highgui
import org.opencv.objdetect.CascadeClassifier

class FaceDetector {
  def run(): Unit = {
    println("Running FaceDetector!")

    val faceDetector = new CascadeClassifier(
      getClass.getResource("/lbpcascade_frontalface.xml").getPath)
    val image = Highgui.imread(getClass.getResource("/lena.png").getPath)

    val faceDetections = new MatOfRect()
    faceDetector.detectMultiScale(image, faceDetections)

    println(s"Detected ${faceDetections.toArray.size} faces.")

    faceDetections.toArray.map { rect =>
      Core.rectangle(
        image,
        new Point(rect.x, rect.y),
        new Point(rect.x + rect.width, rect.y + rect.height),
        new Scalar(0, 255, 0))
    }

    val filename = "faceDetection.png"
    Highgui.imwrite(filename, image)
  }
}
