/**
 *
 */
package org.mmarini.actd.samples

import java.io.File

import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.Environment
import org.mmarini.actd.Feedback
import org.mmarini.actd.TDAgent
import org.mmarini.actd.TDParms
import com.typesafe.scalalogging.LazyLogging
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvwrite
import breeze.stats.distributions.RandBasis
import scala.math.pow

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object FilteredWallTraceApp extends App with LazyLogging {

  val file = "data/debug-wall.csv"
  val EpisodeCount = 30
  val RowIdx = 0
  val ColIdx = 1
  val RowSpeedIdx = 2
  val ColSpeedIdx = 3
  val PadIdx = 4
  val ActionIdx = 5

  /*
     * Filter on the following status:
     *
     *   8 |     o .  |
     *   9 |      O   |
     *  10 |   ---    |
     *      0123456789
     */
  private def filter1(x: DenseVector[Double]) =
    x(RowIdx) == 9 &&
      x(ColIdx) == 6 &&
      x(RowSpeedIdx) == 1 &&
      x(ColSpeedIdx) == -1 &&
      x(PadIdx) == 3

  /*
     * Filter on the following status:
     *
     *   8 |       O  |
     *   9 |      o   |
     *  10 |  ---o    |
     *      0123456789
     */
  private def filter(x: DenseVector[Double]) =
    x(RowIdx) == 8 &&
      x(ColIdx) == 7 &&
      x(RowSpeedIdx) == 1 &&
      x(ColSpeedIdx) == -1 &&
      x(PadIdx) == 2

  /*
     * Filter on the following status
     *
     *   8  O
     *   9   o
     *  10    o---
     *      234567
     */
  private def filter2(x: DenseVector[Double]) =
    x(RowIdx) == 8 && x(ColIdx) == 2 && x(RowSpeedIdx) == 1 && x(ColSpeedIdx) == 1 && x(PadIdx) == 5

  val initEnv = WallStatus.environment

  private def extractSample: Stream[DenseVector[Double]] =
    initEnv.toStream.map {
      case (e0, e1, Feedback(_, action, reward, _)) =>
        val WallStatus((r0, c0), (sr0, sc0), pad0) = e0.status.asInstanceOf[WallStatus]
        val WallStatus((r1, c1), (sr1, sc1), pad1) = e1.status.asInstanceOf[WallStatus]
        val s0 = e0.status.toDenseVector
        val s1 = e1.status.toDenseVector
        val s00v = e0.agent.asInstanceOf[TDAgent].critic(s0).output
        val s01v = e0.agent.asInstanceOf[TDAgent].critic(s1).output
        val p0 = e0.agent.asInstanceOf[TDAgent].actor(s0).output
        val s10v = e1.agent.asInstanceOf[TDAgent].critic(s0).output
        val p1 = e1.agent.asInstanceOf[TDAgent].actor(s0).output
        DenseVector.vertcat(DenseVector(r0, c0, sr0, sc0, pad0,
          action.toDouble, reward,
          r1, c1, sr1, sc1, pad1),
          s00v, s01v, p0,
          s10v, p1)
    }

  /** Generates the report */
  private def generateReport: DenseMatrix[Double] = {
    def trace(s: Stream[DenseVector[Double]], msg: String = "", step: Int = 1): Stream[DenseVector[Double]] =
      s.zipWithIndex.map {
        case (x, i) =>
          if (i % step == 0) logger.info(f"$msg%s - $i%d")
          x
      }
    val stream = trace(trace(extractSample, "Sample", 1000).filter(filter).take(EpisodeCount), "Filtered")

    val out = stream.toArray
    DenseVector.horzcat(out: _*).t
  }

  csvwrite(new File(file), generateReport)
}
