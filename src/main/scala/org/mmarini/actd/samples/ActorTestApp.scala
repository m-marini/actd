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

import scala.annotation.tailrec

import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status
import org.mmarini.actd.ACAgent
import org.mmarini.actd.TDParms

import com.typesafe.scalalogging.LazyLogging

import MDP3Status._
import MDP3Status.MDP3Action._
import breeze.linalg._
import breeze.numerics.exp
import breeze.stats.distributions.RandBasis

/**
 * Tests the agent actor among different parameters
 * and generates the status action preferences reports as octave data file
 */
object ActorTestApp extends App with LazyLogging {

  val file = "data/actor.csv"

  val Beta = 0.1
  val Gamma = 0.9
  val Epsilon = 0.1
  val Lambda = 0.0
  val Eta = 1e-1
  val Seed = 123L
  val Iteration = 300

  val SV2 = 0.0
  val SV1 = 1.0
  val SV0 = -1.0 + SV1 * Gamma

  val A0 = 0
  val A1 = 1

  val svMap = Map(
    (S0, SV0),
    (S1, SV1),
    (S2, SV2))

  val testAgent =
    ACAgent(
      TDParms(
        beta = Beta,
        gamma = Gamma,
        epsilon = 0.0,
        lambda = Lambda,
        eta = Eta,
        l1 = 0,
        l2 = 0,
        random = new RandBasis(new MersenneTwister(Seed))),
      2, 2)

  //  val testAgent = List(0.0, 0.1, 0.2, 0.3).map(p =>
  //    TDAgent(
  //      TDParms(Alpha, Beta, Gamma, Epsilon, p, Eta, new RandBasis(new MersenneTwister(Seed))),
  //      Seq(2, 1), Seq(2, 2), 1.0))

  /**
   *  Tests by iterating on set of episodes
   *
   * @param agent the initial agent
   * @return the list of errors
   */
  private def learn(agent: ACAgent): List[ACAgent] =
    (1 to Iteration).foldLeft(List(agent))((list, _) => {
      createEpisode(list)
    }).reverse

  /**
   * Creates by an episode
   *
   *  @agent the initial agent
   *  @return the trained agent and the error value
   */
  private def createEpisode(list: List[ACAgent]): List[ACAgent] = {
    @tailrec
    def generateEpisode(list: List[ACAgent], status: Status): List[ACAgent] = {
      val (next, feedback) = learnStep(list.head, status)
      val list1 = next :: list
      if (feedback.s1.finalStatus) {
        list1
      } else {
        generateEpisode(list1, feedback.s1)
      }
    }

    generateEpisode(list.head :: list.tail, S0)
  }

  /**
   * Learns by single step
   *
   *  @agent the initial agent
   *  @s0 the initial status
   *  @return the trained agent and the next status and reward
   */
  private def learnStep(agent: ACAgent, s0: Status): (ACAgent, Feedback) = {
    val a = agent.action(s0)
    val feedback = s0.apply(a)
    val (ag1, _) = agent.train(feedback)
    (ag1, feedback)
  }

  /** Saves the result to octave data file */
  private def save(data: DenseMatrix[Double]) {
    csvwrite(new File(file), data)
  }

  /** Runs the test */
  private def test: DenseMatrix[Double] = {
    val x = learn(testAgent)
    val y = x.map(preference)
    val z = DenseMatrix.vertcat(y: _*)
    z
  }

  private def preference(ag: ACAgent): DenseMatrix[Double] = {
    val x = for {
      s <- Array(S0, S1, S2)
    } yield {
      val p = ag.actor(s.toDenseVector).output
      val ep = exp(p)
      val p1 = ep / sum(ep)
      p1.toArray
    }
    val y = x.flatten
    new DenseMatrix(1, y.length, y)
  }

  save(test)
}
