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

import java.io.File
import org.apache.commons.math3.random.MersenneTwister
import com.typesafe.scalalogging.LazyLogging
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvwrite
import breeze.stats.distributions.RandBasis
import org.mmarini.actd.TDAgent
import org.mmarini.actd.TDParms
import MDP3Status._
import MDP3Status.MDP3Action._

/**
 * Tests the agent critics among different parameters
 * and generates the error reports as octave data file
 */
object CriticTestApp extends App with LazyLogging {

  val file = "data/critic.csv"

  val Beta = 0.1
  val Gamma = 0.9
  val Epsilon = 0.1
  val Eta = 1e-1
  val Seed = 123L
  val Iteration = 100
  val SV2 = 0.0
  val SV1 = 1.0
  val SV0 = -1.0 + SV1 * Gamma

  val episodes = Seq(S0.apply(A1.id), S1.apply(A1.id), S2.apply(A1.id))

  val svMap = Map(
    (S0, SV0),
    (S1, SV1))

  val testAgent = List(0.0, 0.01, 0.03, 0.1, 0.3).map(x =>
    TDAgent(
      TDParms(
        beta = Beta,
        gamma = Gamma,
        epsilon = 0.0,
        lambda = x,
        eta = Eta,
        l1 = 0,
        l2 = 0,
        random = new RandBasis(new MersenneTwister(Seed))),
      2, 2))

  /** Computes the error of an agent */
  private def error(agent: TDAgent): Double =
    svMap.map { case (s, e) => e - agent.critic(s.toDenseVector).output(0) }.map(x => x * x).sum

  /**
   *  Tests by iterating on set of episodes
   *
   * @param agent the initial agent
   * @return the list of errors
   */
  private def learn(agent: TDAgent): DenseVector[Double] = {
    val (_, errors) = (1 to Iteration).foldLeft((agent, List(error(agent)))) {
      case ((agent, errs), _) =>
        val (ag, err) = learnEpisode(agent)
        (ag, err :: errs)
    }

    val e1 = errors.reverse
    val delta = new DenseVector(e1.toArray)
    delta
  }

  /**
   * Learns by an episode
   *
   *  @agent the initial agent
   *  @return the trained agent and the error value
   */
  private def learnEpisode(agent: TDAgent): (TDAgent, Double) = {
    val ag = episodes.foldLeft(agent)((agent, step) => {
      val (ag1, _) = agent.train(step)
      ag1
    })
    (ag, error(ag))
  }

  /** Runs the test */
  private def test: DenseMatrix[Double] = {
    val y = testAgent.map(learn(_).toDenseMatrix.t)
    DenseMatrix.horzcat(y: _*)
  }

  csvwrite(new File(file), test)
}
