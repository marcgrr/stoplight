package hmm

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Poisson
import breeze.stats.distributions.Uniform
import com.atul.JavaOpenCV.Imshow
import org.opencv.core.Core
import org.opencv.core.CvType
import org.opencv.core.Mat
import org.opencv.core.MatOfDouble
import org.opencv.core.Scalar
import org.opencv.core.Size
import org.opencv.imgproc.Imgproc

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.util.Random
import scala.collection.JavaConversions._

trait StateSpace[T] {
  def maxp(particles: List[T]): T
  def sample(count: Int): List[T]
}

object StateSpace {
  val imshow = new Imshow("Hello world!")

  def delegateStateSpace[S, T](
      delegate: StateSpace[S],
      constructor: S => T,
      destructor: T => Option[S]): StateSpace[T] = {
    new StateSpace[T] {
      override def maxp(particles: List[T]) = {
        constructor(delegate.maxp(particles.map(destructor).map(_.get)))
      }
      override def sample(count: Int): List[T] = delegate.sample(count).map(constructor)
    }
  }

  def rangeStateSpace(range: Double): StateSpace[Double] = {
    new StateSpace[Double] {
      override def maxp(particles: List[Double]): Double = {
        val expand = 1.0
        val bucketCount = (expand * range).toInt + 1
        val mat = new Mat(new Size(bucketCount, 1), CvType.CV_64FC1)
        mat.setTo(new Scalar(0.0))
        particles.foreach { particle =>
          val intParticle = (expand * particle).toInt
          if (intParticle >= 0 && intParticle < bucketCount - 1) {
            val submat = mat.submat(0, 1, intParticle, intParticle + 1)
            val was = Core.sumElems(submat)
            submat.setTo(new Scalar(was.`val`(0) + 1.0))
          }
        }
        Imgproc.GaussianBlur(mat, mat, new Size(0, 0), 10.0 * expand * range / particles.size, 1.0)
        Core.minMaxLoc(mat).maxLoc.x / expand
      }
      override def sample(count: Int): List[Double] = {
        val distribution = new Uniform(0.0, range)
        distribution.sample(count).toList
      }
    }
  }

  def rangeStateSpace(xRange: Double, yRange: Double): StateSpace[(Double, Double)] = {
    new StateSpace[(Double, Double)] {
      override def maxp(particles: List[(Double, Double)]): (Double, Double) = {
        val expand = 30.0
        val xBucketCount = (expand * xRange).toInt + 1
        val yBucketCount = (expand * yRange).toInt + 1
        val mat = new Mat(new Size(xBucketCount, yBucketCount), CvType.CV_64FC1)
        mat.setTo(new Scalar(0.0))
        particles.foreach { particle =>
          val xIntParticle = (expand * particle._1).toInt
          val yIntParticle = (expand * particle._2).toInt
          if (xIntParticle >= 0 && xIntParticle < xBucketCount - 1 && yIntParticle >= 0 && yIntParticle < yBucketCount - 1) {
            val submat = mat.submat(yIntParticle, yIntParticle + 1, xIntParticle, xIntParticle + 1)
            val was = Core.sumElems(submat)
            submat.setTo(new Scalar(was.`val`(0) + 1.0))
          }
        }
        Imgproc.GaussianBlur(mat, mat, new Size(0, 0), 1.0 * expand * xRange / Math.sqrt(particles.size), 1.0 * expand * yRange / Math.sqrt(particles.size))
        val maxLoc = Core.minMaxLoc(mat).maxLoc
        println(Core.minMaxLoc(mat).maxVal)

        {
          val maxVal = Core.minMaxLoc(mat).maxVal
          val minVal = Core.minMaxLoc(mat).minVal

          val showMat = new Mat
          mat.convertTo(showMat, CvType.CV_8UC1, 255.0/(maxVal - minVal), -minVal * 255.0/(maxVal - minVal))
          Imgproc.cvtColor(showMat, showMat, Imgproc.COLOR_GRAY2BGR)
          imshow.showImage(showMat)
        }
        (maxLoc.x / expand, maxLoc.y / expand)
      }
      override def sample(count: Int): List[(Double, Double)] = {
        val xDistribution = new Uniform(0.0, xRange)
        val yDistribution = new Uniform(0.0, yRange)
        xDistribution.sample(count).zip(yDistribution.sample(count)).toList
      }
    }
  }

  /**
   * Danger: maxp finds marginal MAP for each component separately!
   */
  def tupleStateSpace[T1, T2](s1: StateSpace[T1], s2: StateSpace[T2]): StateSpace[(T1, T2)] = {
    new StateSpace[(T1, T2)] {
      override def maxp(particles: List[(T1, T2)]) = {
        val t1 = s1.maxp(particles.map(_._1))
        val t2 = s2.maxp(particles.map(_._2))
        (t1, t2)
      }
      override def sample(count: Int): List[(T1, T2)] = {
        s1.sample(count).zip(s2.sample(count))
      }
    }
  }

  /**
   * Danger: maxp finds marginal MAP for each component separately!
   */
  def tupleStateSpace[T1, T2, T3](s1: StateSpace[T1], s2: StateSpace[T2], s3: StateSpace[T3]):
    StateSpace[(T1, T2, T3)] = {

    new StateSpace[(T1, T2, T3)] {
      override def maxp(particles: List[(T1, T2, T3)]) = {
        val t1 = s1.maxp(particles.map(_._1))
        val t2 = s2.maxp(particles.map(_._2))
        val t3 = s3.maxp(particles.map(_._3))
        (t1, t2, t3)
      }
      override def sample(count: Int): List[(T1, T2, T3)] = {
        s1.sample(count).zip(s2.sample(count)).zip(s3.sample(count)).map { case ((a, b), c) =>
          (a, b, c)
        }
      }
    }
  }

  def unionStateSpace[L, T1 <: L, T2 <: L](s1: StateSpace[T1], s2: StateSpace[T2])
      (implicit t1Tag: ClassTag[T1], t2Tag: ClassTag[T2]): StateSpace[L] = {

    new StateSpace[L] {
      override def maxp(particles: List[L]): L = {
        val p1 = particles.collect {
          case t1: Any if t1Tag.runtimeClass.isInstance(t1) => t1.asInstanceOf[T1]
        }
        val p2 = particles.collect {
          case t2: Any if t2Tag.runtimeClass.isInstance(t2) => t2.asInstanceOf[T2]
        }
        val p1s = p1.size
        val p2s = p2.size
        val maxs = List(p1s, p2s).max
        if (p1s == maxs) {
          s1.maxp(p1)
        } else {
          s2.maxp(p2)
        }
      }
      override def sample(count: Int): List[L] = {
        (0 until count).flatMap { _ =>
          Random.nextInt(2) match {
            case 0 => s1.sample(1)
            case 1 => s2.sample(1)
          }
        }
      }.toList
    }
  }

  def unionStateSpace[L, T1 <: L, T2 <: L, T3 <: L](
      s1: StateSpace[T1], s2: StateSpace[T2], s3: StateSpace[T3])
      (implicit t1Tag: ClassTag[T1], t2Tag: ClassTag[T2], t3Tag: ClassTag[T3]): StateSpace[L] = {

    new StateSpace[L] {
      override def maxp(particles: List[L]): L = {
        val p1 = particles.collect {
          case t1: Any if t1Tag.runtimeClass.isInstance(t1) => t1.asInstanceOf[T1]
        }
        val p2 = particles.collect {
          case t2: Any if t2Tag.runtimeClass.isInstance(t2) => t2.asInstanceOf[T2]
        }
        val p3 = particles.collect {
          case t3: Any if t3Tag.runtimeClass.isInstance(t3) => t3.asInstanceOf[T3]
        }
        val p1s = p1.size
        val p2s = p2.size
        val p3s = p3.size
        val maxs = List(p1s, p2s, p3s).max
        if (p1s == maxs) {
          s1.maxp(p1)
        } else if (p2s == maxs) {
          s2.maxp(p2)
        } else {
          s3.maxp(p3)
        }
      }
      override def sample(count: Int): List[L] = {
        (0 until count).flatMap { _ =>
          Random.nextInt(3) match {
            case 0 => s1.sample(1)
            case 1 => s2.sample(1)
            case 2 => s3.sample(1)
          }
        }
      }.toList
    }
  }
}

sealed trait LightState
case class Green(time: Double) extends LightState
case class Red(time: Double) extends LightState

sealed trait RuleState
case class Regular(greenTime: Double, redTime: Double) extends RuleState
case class TriggeredGreen(greenTime: Double) extends RuleState
case class TriggeredRed(redTime: Double) extends RuleState

case class FullState(
    lightState: LightState,
    ruleState: RuleState) {

  def thingHappened(happenRate: Double, timeStep: Double): Boolean = {
    val poission = new Poisson(happenRate * timeStep)
    poission.sample >= 1
  }

  def lightSwitched(time: Double, timeStep: Double, switchTime: Double): Boolean = {
    val switchRate = if (time < switchTime - 5.0) {
      0.001
    } else if (time < switchTime - 1.0) {
      0.1
    } else if (time < switchTime + 1.0) {
      0.5
    } else {
      2.0
    }
    thingHappened(switchRate, timeStep)
  }

  def sampleNext(count: Int, timeStep: Double): List[FullState] = {
    (0 until count).map { _ =>
      // Advance the light state.
      val newLightState = ruleState match {
        case Regular(greenTime, redTime) =>
          lightState match {
            case Green(time) =>
              if (lightSwitched(time, timeStep, greenTime)) {
                Red(0.0)
              } else {
                Green(time + timeStep)
              }
            case Red(time) =>
              if (lightSwitched(time, timeStep, redTime)) {
                Green(0.0)
              } else {
                Red(time + timeStep)
              }
          }

        case TriggeredGreen(greenTime) =>
          lightState match {
            case Green(time) =>
              if (lightSwitched(time, timeStep, greenTime)) {
                Red(0.0)
              } else {
                Green(time + timeStep)
              }
            case Red(time) =>
              val switchRate = 0.01
              if (thingHappened(switchRate, timeStep)) {
                Green(0.0)
              } else {
                Red(time + timeStep)
              }
          }

        case TriggeredRed(redTime) =>
          lightState match {
            case Green(time) =>
              val switchRate = 0.01
              if (thingHappened(switchRate, timeStep)) {
                Red(0.0)
              } else {
                Green(time + timeStep)
              }
            case Red(time) =>
              if (lightSwitched(time, timeStep, redTime)) {
                Green(0.0)
              } else {
                Red(time + timeStep)
              }
          }
      }

      // Mix the rule state.
      val ruleChangeRate = 0.001
      val ruleTimeChangeRate = 0.1
      val ruleTimeDeltas = new Gaussian(0.0, ruleTimeChangeRate * timeStep).sample(2)
      val newRuleState = ruleState match {
        case Regular(greenTime, redTime) =>
          if (thingHappened(ruleChangeRate, timeStep)) {
            if (Random.nextInt(2) == 0) {
              TriggeredGreen(greenTime)
            } else {
              TriggeredRed(redTime)
            }
          } else {
            Regular(greenTime + ruleTimeDeltas(0), redTime + ruleTimeDeltas(1))
          }
        case TriggeredGreen(greenTime) =>
          if (thingHappened(ruleChangeRate, timeStep)) {
            if (Random.nextInt(2) == 0) {
              Regular(greenTime, greenTime)
            } else {
              TriggeredRed(greenTime)
            }
          } else {
            TriggeredGreen(greenTime + ruleTimeDeltas(0))
          }
        case TriggeredRed(redTime) =>
          if (thingHappened(ruleChangeRate, timeStep)) {
            if (Random.nextInt(2) == 0) {
              Regular(redTime, redTime)
            } else {
              TriggeredGreen(redTime)
            }
          } else {
            TriggeredRed(redTime + ruleTimeDeltas(0))
          }
      }

      FullState(newLightState, newRuleState)
    }.toList
  }
}

object FullState {
  def space(range: Double): StateSpace[FullState] = {
    val lightStateSpace = {
      val rangeStateSpace = StateSpace.rangeStateSpace(range)
      val greenStateSpace =
        StateSpace.delegateStateSpace(rangeStateSpace, Green.apply, Green.unapply)
      val redStateSpace =
        StateSpace.delegateStateSpace(rangeStateSpace, Red.apply, Red.unapply)
      StateSpace.unionStateSpace[LightState, Green, Red](greenStateSpace, redStateSpace)
    }

    val ruleStateSpace = {
      val rangeStateSpace = StateSpace.rangeStateSpace(range, range)
      val rangeStateSpace2 = StateSpace.rangeStateSpace(range)
      val regularStateSpace =
        StateSpace.delegateStateSpace(rangeStateSpace, (Regular.apply _).tupled, Regular.unapply)
      val triggeredGreenStateSpace =
        StateSpace.delegateStateSpace(rangeStateSpace2, (TriggeredGreen.apply _), TriggeredGreen.unapply)
      val triggeredRedStateSpace =
        StateSpace.delegateStateSpace(rangeStateSpace2, (TriggeredRed.apply _), TriggeredRed.unapply)
      StateSpace.unionStateSpace[RuleState, Regular, TriggeredGreen, TriggeredRed](
        regularStateSpace,
        triggeredGreenStateSpace,
        triggeredRedStateSpace)
    }

    StateSpace.delegateStateSpace(
      StateSpace.tupleStateSpace(lightStateSpace, ruleStateSpace),
      (FullState.apply _).tupled,
      FullState.unapply)
  }
}

object Filter {
  private[this] def weightedSample[T](weighted: List[(Double, T)], count: Int): List[T] = {
    val totalWeight = weighted.map(_._1).sum
    weighted.filter { case (weight, _) =>
      Random.nextDouble < count.toDouble * weight / totalWeight
    }.map { case (_, t) =>
      t
    }.toList
  }

  def step(space: StateSpace[FullState], particleCount: Int, particles: List[FullState], timeStep: Double, observation: String): List[FullState] = {
    val steppedParticles = particles.flatMap(_.sampleNext(20, timeStep))

    val errorParticles = space.sample(particleCount / 10)

    val weightedParticles = (steppedParticles ++ errorParticles).map { particle =>
      val weight = particle.lightState match {
        case Green(_) if observation == "green" => 1.0
        case Red(_) if observation == "red" => 1.0
        case _ => 0.1
      }
      (weight, particle)
    }

    weightedSample(weightedParticles, particleCount)
  }

  def testCase(): Unit = {
    val stateSpace = FullState.space(20.0)
    val particleCount = 10000
    var particles = stateSpace.sample(particleCount)

    val funRules = List(
      Regular(4.0, 14.0),
      Regular(2.0, 18.0),
      Regular(12.0, 4.0),
      TriggeredGreen(4.0),
      TriggeredGreen(2.0),
      TriggeredRed(12.0),
      TriggeredRed(10.0),
      TriggeredRed(4.0),
      TriggeredRed(2.0),
      TriggeredGreen(12.0),
      TriggeredGreen(10.0))

    var trueState = FullState(Green(0.0), Regular(4.0, 14.0))
    var convergedForCycles = 0
    var convergenceTookCycles = 0

    while (true) {
      val observation = trueState.lightState match {
        case Green(_) => "green"
        case Red(_) => "red"
      }
      particles = step(stateSpace, particleCount, particles, 1.0, observation)
      stateSpace.maxp(particles)

      val newLightState = trueState.ruleState match {
        case Regular(greenTime, redTime) =>
          trueState.lightState match {
            case Green(time) =>
              if (time > greenTime) Red(0.0) else Green(time + 1.0)
            case Red(time) =>
              if (time > redTime) Green(0.0) else Red(time + 1.0)
          }
        case TriggeredGreen(greenTime) =>
          trueState.lightState match {
            case Green(time) =>
              if (time > greenTime) Red(0.0) else Green(time + 1.0)
            case Red(time) =>
              if (trueState.thingHappened(0.02, 1.0)) Green(0.0) else Red(time + 1.0)
          }
        case TriggeredRed(redTime) =>
          trueState.lightState match {
            case Green(time) =>
              if (trueState.thingHappened(0.02, 1.0)) Red(0.0) else Green(time + 1.0)
            case Red(time) =>
              if (time > redTime) Green(0.0) else Red(time + 1.0)
          }
      }

      val newRuleState = (trueState.lightState, newLightState) match {
        case (Green(_), Red(_)) | (Red(_), Green(_)) =>
          val guessedState = stateSpace.maxp(particles)
          val isConverged = (trueState.ruleState, guessedState.ruleState) match {
            case (Regular(a1, b1), Regular(a2, b2)) => Math.abs(a1 - a2) < 2.0 && Math.abs(b1 - b2) < 2.0
            case (TriggeredRed(a1), TriggeredRed(a2)) => Math.abs(a1 - a2) < 2.0
            case (TriggeredGreen(a1), TriggeredGreen(a2)) => Math.abs(a1 - a2) < 2.0
            case _ => false
          }
          convergenceTookCycles += 1
          if (isConverged) {
            convergedForCycles += 1
            println(s"Cycle $convergenceTookCycles has been converged for $convergedForCycles.")
            println((guessedState.ruleState, trueState.ruleState))
          } else {
            convergedForCycles = 0
            println(s"Cycle $convergenceTookCycles is not converged.")
            println((guessedState.ruleState, trueState.ruleState))
          }
          if (convergedForCycles >= 5) {
            println(s"Converged in ${convergenceTookCycles - 5} cycles!!")
            convergedForCycles = 0
            convergenceTookCycles = 0
            Random.shuffle(funRules).toList.head
          } else {
            trueState.ruleState
          }
        case _ => trueState.ruleState
      }

      trueState = FullState(newLightState, newRuleState)
    }
  }
}
