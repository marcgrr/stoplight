package io

trait FrameReader {
  def read(): (Int, Frame)
}
