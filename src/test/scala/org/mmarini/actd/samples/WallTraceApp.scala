/**
 *
 */
package org.mmarini.actd.samples

import com.typesafe.scalalogging.LazyLogging

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object WallTraceApp extends App with LazyLogging {

  val file = "data/wall.csv"
  val StepCount = 30000
  val SampleTraceCount = 1000

  WallStatus.environment.iterator.
    toSamplesWithAC.
    trace("Sample", SampleTraceCount).
    take(StepCount).
    write(file)
}
