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

  val file = "data/wall-8.csv"
  val EpisodeCount = 30

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
    /*
     * Filter on
     *
     *   8       O
     *   9      o
     *  10  XXX
     *      234567
     */

    val f = extractSample.filter { x => x(0) == 7 && x(1) == 8 && x(2) == 1 && x(3) == -1 && x(4) == 2 }

    /*
     * Filter on
     *
     *   8  O
     *   9   o 
     *  10     XXX
     *      234567
     */
    //    val f = extractSample.filter { x => x(0) == 8 && x(1) == 2 && x(2) == 1 && x(3) == 1 && x(4) == 5 }

    val out = f.take(EpisodeCount).zipWithIndex.map {
      case (x, i) =>
        logger.info(s"Sample $i")
        x
    }.toArray
    DenseVector.horzcat(out: _*).t
  }

  csvwrite(new File(file), generateReport)
}
