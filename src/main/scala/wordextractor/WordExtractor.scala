package wordextractor

import io.Frame

import scala.collection.immutable

/**
 * Extracts words from frames. Implementations may be stateful!
 */
trait WordExtractor[VisualizationT] {

  /**
   * Get words for a single frame. We might refuse to return words from the frame and return `None`.
   */
  def getWords(frame: Frame): Option[(immutable.Seq[Word], VisualizationT)]

  /**
   * Get words from a sequence of frames. We'll return `None`s for the frames whose words we
   * refuse to compute.
   */
  def getWords(frames: immutable.Seq[Frame]):
    immutable.Seq[Option[(immutable.Seq[Word], VisualizationT)]] = {
    frames.map(getWords)
  }

}
