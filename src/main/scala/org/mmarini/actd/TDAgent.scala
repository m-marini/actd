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

package org.mmarini.actd

import java.io.File

import scala.IndexedSeq

import org.apache.commons.math3.random.MersenneTwister

import breeze.linalg.DenseVector
import breeze.linalg.csvread
import breeze.linalg.sum
import breeze.numerics.exp
import breeze.stats.distributions.RandBasis

/**
 * A learning agent that replies to stimulus with actions and learns by receiving rewards
 * applying TD algorithm.
 *
 * @constructor create a learning agent with parameters, critic network and actor network
 * @parm parms the parameters
 * @parm critic the critic network
 * @parm actor the actor network
 *
 * @author us00852
 */
class TDAgent(
    val parms: TDParms,
    val critic: TDNeuralNet,
    val actor: TDNeuralNet) extends Agent {

  /** Creates a new agent with a new critic network */
  def critic(c: TDNeuralNet): TDAgent = new TDAgent(
    parms = parms,
    critic = c,
    actor = actor)

  /** Returns the action to be taken in a state */
  def action(status: Status): Action =
    parms.indexEGreedyBySoftmax(actor(status.toDenseVector).output)

  /** Returns a new agent that has learned by reward and the error */
  def train(feedback: Feedback): (TDAgent, Double) = {

    // Computes the state value pre and post step
    val s0Vect = feedback.s0.toDenseVector
    val s1Vect = feedback.s1.toDenseVector

    val end0 = feedback.s0.finalStatus
    val end1 = feedback.s1.finalStatus

    // The status value of post state is 0 if final episode else bootstraps from critic
    val postValue = if (end1 || end0) 0.0 else critic(s1Vect).output(0)

    // Computes the expected state value by booting the previous status value */
    val expectedValue = postValue * parms.gamma + feedback.reward

    // Computes the error by critic
    val preValue = critic(s0Vect).output(0)

    val delta = expectedValue - preValue

    // Teaches the critic by evidence
    val cs = critic(s0Vect)
    val nc = cs.train(DenseVector(expectedValue) - cs.output)

    // Computes the expected action preferences applying the critic error to previous decision */
    val as = actor(s0Vect)
    val pref = as.output
    val pexp = exp(pref)
    val prob = pexp / sum(pexp)
    val action = feedback.action
    //    val expectedPref = pref.copy
    ////    expectedPref.update(action, expectedPref(action) + parms.beta * delta * (1 - prob(action)))
    //    expectedPref.update(action, prefs + parms.beta * delta ))

    val errs = DenseVector.zeros[Double](pref.length)
    //    errs.update(action, parms.beta * delta)
    errs.update(action, parms.beta * delta * (1 - prob(action)))

    // Teaches the actor by evidence
    val na = as.train(errs)

    val nag = if (end0) {
      new TDAgent(parms, nc.clearTraces, na.clearTraces)
    } else {
      new TDAgent(parms, nc, na)
    }
    (nag, delta)
  }

  /** Writes a file with agent data */
  def write(file: String) {
    // Saves parameters
    val parm = IndexedSeq(DenseVector[Double](
      parms.beta,
      parms.gamma,
      parms.epsilon,
      parms.lambda,
      parms.eta,
      parms.l1,
      parms.l2))
    parm.iterator.write(s"$file-parms.csv")

    // Saves critic
    critic.write(s"$file-critic")

    // Saves actor
    actor.write(s"$file-actor")
  }
}

/** Factory for [[TDAgent]] instances */
object TDAgent {

  private val LambdaIndex = 3
  private val EtaIndex = 4
  private val L1Index = 5
  private val L2Index = 6
  private val MaxTrainingSamplesIndex = 6

  /**
   * Creates a TDAgent with TD parameter,
   *  hidden layers networks and
   *  weights within a range.
   */
  def apply(
    parms: TDParms,
    statusSize: Int,
    actionCount: Int,
    hiddenLayers: Int*): TDAgent =
    new TDAgent(parms,
      TDNeuralNet(parms)(statusSize +: hiddenLayers :+ 1),
      TDNeuralNet(parms)(statusSize +: hiddenLayers :+ actionCount))

  /**
   * Creates a TDAgent reading from file set
   */
  def apply(file: String, random: RandBasis = new RandBasis(new MersenneTwister())): TDAgent = {
    val p = csvread(new File(s"$file-parms.csv"))
    val parms = TDParms(
      beta = p(0, 0),
      gamma = p(0, 1),
      epsilon = p(0, 2),
      lambda = p(0, LambdaIndex),
      eta = p(0, EtaIndex),
      l1 = p(0, L1Index),
      l2 = p(0, L2Index),
      random)

    val critic = TDNeuralNet.read(parms)(s"$file-critic")
    val actor = TDNeuralNet.read(parms)(s"$file-actor")
    new TDAgent(parms, critic, actor)
  }
}
