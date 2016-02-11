// Copyright (c) 2016 Marco Marini, marco.marini@mmarini.org
//
// Licensed under the MIT License (MIT);
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://opensource.org/licenses/MIT
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

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
  val SampleCount = 100
  val LearningCount = 300

  /** Generates the report */
  val (feedbacks, errs) = WallStatus.
    environment.
    iterator.
    trace("Sample", SampleTraceCount).
    take(SampleCount).
    map {
      case (_, _, f, err) => (f, err)
    }.toList.unzip

  val err0 = errs.map(x => x * x).sum

  val iter = 1 to LearningCount
  val ag0 = WallStatus.environment.agent

  private def learn(ag0: Agent) =
    feedbacks.foldLeft((ag0, 0.0)) {
      case ((agent, sum), feedback) =>
        val (ag, er) = agent.train(feedback)
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
    map(x => DenseVector.apply(x / errors.length)).
    write(file)
}
