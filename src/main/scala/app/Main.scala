package app

import hmm.Filter
import interactive.Visualizer
import io.DirectoryFrameReader
import wordextractor.channelintensity.ChannelIntensityConfiguration
import wordextractor.channelintensity.ChannelIntensityExtractor
import wordextractor.foregroundobjectdirection.ForegroundObjectDirectionConfiguration
import wordextractor.foregroundobjectdirection.ForegroundObjectDirectionExtractor
import org.opencv.core.Core

object Main {
  def main(args: Array[String]): Unit = {
    System.loadLibrary(Core.NATIVE_LIBRARY_NAME)
    //new WebcamWatcher("central-rengstorff-eb", "http://ie.trafficland.com/401822/full?system=santaclara&pubtoken=def997067f147290171f9e54f36e64defdc").watch()
    //new ChannelSplitter("central-rengstorff-eb").run()
    //new Foreground("central-rengstorff-eb", "http://ie.trafficland.com/401822/full?system=santaclara&pubtoken=deffab27e83a64a909b545e037a75591e75&1430782690083").run()

    //val frameReader = new DirectoryFrameReader("frames/sunset")
    /*val wordExtractor = new ForegroundObjectDirectionExtractor(
      ForegroundObjectDirectionConfiguration(
        0.02,
        10,
        1,
        1))*/
    /*val wordExtractor = new ChannelIntensityExtractor(
      ChannelIntensityConfiguration(
        1,
        10,
        10,
        10))
    val visualizer = new Visualizer(frameReader, wordExtractor)
    visualizer.run()*/
    Filter.testCase()
  }
}
