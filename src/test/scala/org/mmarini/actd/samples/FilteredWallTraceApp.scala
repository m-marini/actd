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
  val EpisodeCount = 100
  val SampleTraceCount = 1000


  import WallTestStreams._
  /*
     * Filter on the following status:
     *
     *   8 |     o .  |
     *   9 |      O   |
     *  10 |   ---    |
     *      0123456789
     */
  private def filter3(x: DenseVector[Double]) =
    x(RowIdx) == 9 &&
      x(ColIdx) == 6 &&
      x(RowSpeedIdx) == 1 &&
      x(ColSpeedIdx) == -1 &&
      x(PadIdx) == 3

  /*
     * Filter on the following status:
     *
     *   8 |       .  |
     *   9 |      O   |
     *  10 |  ---o    |
     *      0123456789
     */
  private def filter1(x: DenseVector[Double]) =
    x(RowIdx) == 9 &&
      x(ColIdx) == 6 &&
      x(RowSpeedIdx) == 1 &&
      x(ColSpeedIdx) == -1 &&
      x(PadIdx) == 2

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

  /** Generates the report */
  private def generateReport: DenseMatrix[Double] = {
    val s1 = WallTestStreams.toSamplesWithAC(WallStatus.environment.toStream)
    val s2 = TestStreams.trace(s1, "Sample", SampleTraceCount)
    val s3 = TestStreams.trace(s2.filter(filter), "Filtered");
    val stream = s3.take(EpisodeCount)

    val out = stream.toArray
    DenseVector.horzcat(out: _*).t
  }

  csvwrite(new File(file), generateReport)
}
