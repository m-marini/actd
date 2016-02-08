/**
 *
 */
package org.mmarini.actd.samples

import com.typesafe.scalalogging.LazyLogging
import breeze.linalg.DenseVector
import Indexes._
import org.mmarini.actd.Feedback
import org.mmarini.actd.Agent
import breeze.linalg.operators.DenseVector_GenericOps

/**
 * Generate a report of a single episode
 */
object EpisodeTestApp extends App with LazyLogging {

  val file = "data/episode-wall.csv"
  val SampleTraceCount = 1000
  val LearningCount = 1000

  /** Generates the report */
  val (feedbacks, errs) = WallStatus.
    environment.
    iterator.
    trace("Sample", SampleTraceCount).
    takeWhile {
      case (_, _, Feedback(s0, _, _, _), _) => !s0.finalStatus
    }.
    map {
      case (_, _, f, err) => (f, err)
    }.toList.unzip

  val err0 = errs.map(x => x * x).sum

  val iter = 1 to LearningCount
  val ag0 = WallStatus.environment.agent

  private def learn(ag0: Agent) =
    feedbacks.foldLeft((ag0, 0.0)) {
      case ((agent, sum), feedback) =>
        val (ag, er) = agent.learn(feedback)
        (ag, sum + er * er)
    }

  val (_, errors) = iter.foldLeft((ag0, Seq(err0))) {
    case ((ag, list), _) =>
      val (ag1, err) = learn(ag)
      (ag1, err +: list)
  }

  errors.
    reverse.
    iterator.
    map(x => DenseVector.apply(x)).
    write(file)
}
