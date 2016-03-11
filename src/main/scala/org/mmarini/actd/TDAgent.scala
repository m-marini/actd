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

import breeze.linalg.DenseVector
import breeze.io.CSVWriter

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
    val nc = critic.learn(s0Vect, DenseVector(expectedValue))

    // Computes the expected action preferences applying the critic error to previous decision */
    val pref = actor(s0Vect).output
    val expectedPref = pref.copy
    val action = feedback.action
    expectedPref(action to action) += parms.beta * delta

    // Teaches the actor by evidence
    val na = actor.learn(s0Vect, expectedPref)

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
      parms.alpha,
      parms.beta,
      parms.gamma,
      parms.epsilon,
      parms.lambda,
      parms.eta,
      parms.maxTrainingSamples.toDouble))
    parm.iterator.write(s"$file-parms.csv")

    // Saves critic
    critic.weights.write(s"$file-critic")

    // Saves actor
    actor.weights.write(s"$file-actor")
  }
}

/** Factory for [[TDAgent]] instances */
object TDAgent {

  /**
   * Creates a TDAgent with TD parameter,
   *  hidden layers networks and
   *  weights within a range.
   */
  def apply(
    parms: TDParms,
    sigma: Double,
    statusSize: Int,
    actionCount: Int,
    hiddenLayers: Int*): TDAgent =
    new TDAgent(parms,
      TDNeuralNet(statusSize +: hiddenLayers :+ 1, parms, sigma),
      TDNeuralNet(statusSize +: hiddenLayers :+ actionCount, parms, sigma))

}
