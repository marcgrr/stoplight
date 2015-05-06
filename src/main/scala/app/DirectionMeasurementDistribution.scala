package app

/**
 * Represents the distribution of direction measurements you might make if the true directions are
 * distributed uniformly between `lowerAngle` and `upperAngle` (going clockwise).
 */
case class DirectionMeasurementDistribution(
    lowerAngle: Double,
    upperAngle: Double,
    boundaryFalloff: Double,
    uniformErrorDensity: Double) {

  require(lowerAngle >= 0.0)
  require(lowerAngle < 360.0)
  require(upperAngle >= 0.0)
  require(upperAngle < 360.0)
  require(uniformErrorDensity >= 0.0)
  require(boundaryFalloff >= 0.0)

  def density(measuredAngle: Double): Double = {
    require(measuredAngle >= 0.0)
    require(measuredAngle < 360)

    if (spinnyDistance(measuredAngle, lowerAngle) < spinnyDistance(measuredAngle, upperAngle)) {
      // Outside the range! Calculate the falloff!
      val contributionFromLower = 1.0 - spinnyDistance(measuredAngle, lowerAngle) / boundaryFalloff
      val contributionFromUpper = 1.0 - spinnyDistance(upperAngle, measuredAngle) / boundaryFalloff
      List(0.0, contributionFromLower, contributionFromUpper).max + uniformErrorDensity
    } else {
      1.0 + uniformErrorDensity
    }
  }

  private[this] def spinnyDistance(from: Double, to: Double): Double = {
    if (from <= to) {
      to - from
    } else {
      to - from + 360
    }
  }

}
