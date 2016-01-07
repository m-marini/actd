/**
 *
 */
package org.mmarini.actd.samples

import com.typesafe.scalalogging.LazyLogging

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object WallBenchmarkApp extends App with LazyLogging {

  val file = "data/benchmark-wall.csv"
  val StepCount = 300000
  val SampleTraceCount = 10000

  /** Generates the report */
  WallStatus.dummyEnv.iterator.
    toSamples.
    trace("Sample", SampleTraceCount).
    take(StepCount).
    write(file)
}
