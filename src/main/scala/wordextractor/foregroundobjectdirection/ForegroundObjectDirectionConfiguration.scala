package wordextractor.foregroundobjectdirection

case class ForegroundObjectDirectionConfiguration(
    backgroundSubtractorLearningRate: Double,
    directionBucketCount: Int,
    xBucketCount: Int,
    yBucketCount: Int)
