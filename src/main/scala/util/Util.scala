package util

import scala.math.floor

object Util {

  def bucket(value: Double, min: Double, max: Double, bucketCount: Int): Int = {
    require(min < max)
    require(bucketCount > 0)
    val result = floor(bucketCount.toDouble * (value - min) / (max - min)).toInt
    if (result < 0) 0 else if (result >= bucketCount) bucketCount - 1 else result
  }

}