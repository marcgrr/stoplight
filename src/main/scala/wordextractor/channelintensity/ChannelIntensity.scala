package wordextractor.channelintensity

import wordextractor.Word

case class ChannelIntensity(
    configuration: ChannelIntensityConfiguration,
    intensityBucket: Int,
    xBucket: Int,
    yBucket: Int) extends Word
