package app

case class LightProbability(green: Double, red: Double) {

  def update(greenUpdate: Double, redUpdate: Double): LightProbability = {
    val unnormalizedGreen = green * greenUpdate
    val unnormalizedRed = red * redUpdate
    val normalizaton = 1.0 / (unnormalizedGreen + unnormalizedRed)
    LightProbability(normalizaton * unnormalizedGreen, normalizaton * unnormalizedRed)
  }

  def stepTime(frequency: Double): LightProbability = {
    val switchProbability = 1.0 / frequency
    LightProbability(
      (1 - switchProbability) * green + switchProbability * red,
      switchProbability * green + (1 - switchProbability) * red)
  }

}