package wordextractor.foregroundobjectdirection

import wordextractor.Word

case class ForegroundObjectDirection(
    configuration: ForegroundObjectDirectionConfiguration,
    directionBucket: Int,
    xBucket: Int,
    yBucket: Int) extends Word
