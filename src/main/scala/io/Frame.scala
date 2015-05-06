package io

import org.joda.time.DateTime
import org.opencv.core.Mat

case class Frame(capturedAt: DateTime, image: Mat)
