/**
 *
 */
package org.mmarini.actd.samples

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg.DenseVector

import Indexes._

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object FilteredWallTraceApp extends App with LazyLogging {

  val file = "data/debug-wall.csv"
  val EpisodeCount = 100
  val SampleTraceCount = 1000

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
  WallStatus.environment.iterator.
    toSamplesWithAC.
    trace("Sample", SampleTraceCount).
    filter(filter).
    trace("Filtered").
    take(EpisodeCount).
    write(file)
}
