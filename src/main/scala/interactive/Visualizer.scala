package interactive

import com.atul.JavaOpenCV.Imshow
import io.FrameReader
import wordextractor.WordExtractor
import org.opencv.core.Core
import org.opencv.core.Mat

import scala.collection.JavaConversions._


class Visualizer(frameReader: FrameReader, wordExtractor: WordExtractor[Mat]) {
  var index = -1

  def run(): Unit = {
    val imshow = new Imshow("Hello World!")

    // HAXX!
    frameReader.read()
    frameReader.read()

    while (true) {
      val (newIndex, frame) = frameReader.read()
      if (newIndex > index) {
        index = newIndex

        wordExtractor.getWords(frame) match {
          case Some((words, visualization)) =>
            val image = new Mat
            Core.hconcat(List(frame.image, visualization), image)
            imshow.showImage(image)
            println(words)
            Thread.sleep(200)
          case None =>
            println("No output from WordExtractor. Going to next frame...")
        }

      } else {
        println("Didn't see new frame. Sleeping...")
        Thread.sleep(200)
      }
    }
  }
}
