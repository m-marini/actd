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
import breeze.linalg.max
import breeze.stats.distributions.RandBasis

/**
 * A learning agent that replies to stimulus with actions and learns by receiving rewards
 * applying TD algorithm.
 *
 * @constructor create a learning agent with parameters, q function network
 * @parm parms the parameters
 * @parm qFunction the q function network
 *
 * @author us00852
 */
class QAgent(
    override val parms: TDParms,
    val qFunction: TDNeuralNet) extends Agent {

  /** Returns the action to be taken in a state */
  def action(status: Status): Action = {
    parms.indexEGreedy(qFunction(status.toDenseVector).output)
  }

  private def qf(s: Status): DenseVector[Double] =
    if (s.finalStatus) {
      DenseVector.zeros[Double](qFunction(s.toDenseVector).output.length)
    } else {
      qFunction(s.toDenseVector).output
    }

  /** Returns a new agent that has learned by reward and the error */
  def train(feedback: Feedback): (Agent, Double) = {

    // Computes the action value pre and post step
    val s0Vect = feedback.s0.toDenseVector
    val s1Vect = feedback.s1.toDenseVector

    val qs0 = qf(feedback.s0)
    val qs0a = max(qs0)

    val qs1 = qf(feedback.s1)
    val qs1a = max(qs1)

    val action = feedback.action

    val q1s0a = feedback.reward + parms.gamma * qs1a

    val q1s0 = qs0.copy
    q1s0.update(action, q1s0a)

    val delta = q1s0 - qs0
    val qf1 = if (qs0(action) == qs0a) qFunction else qFunction.clearTraces
    val qf2 = qf1(s0Vect).train(delta)

    val nag = if (feedback.s0.finalStatus) {
      new QAgent(parms, qf2.clearTraces)
    } else {
      new QAgent(parms, qf2)
    }
    (nag, q1s0a - qs0a)

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

    // Saves qFunction
    qFunction.write(s"$file-q")
  }
}

/** Factory for [[QAgent]] instances */
object QAgent {

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
    hiddenLayers: Int*): QAgent =
    new QAgent(parms,
      TDNeuralNet(parms)(statusSize +: hiddenLayers :+ actionCount))

  /**
   * Creates a TDAgent reading from file set
   */
  def apply(file: String, random: RandBasis = new RandBasis(new MersenneTwister())): QAgent = {
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

    val qFunc = TDNeuralNet.read(parms)(s"$file-q")
    new QAgent(parms, qFunc)
  }
}
