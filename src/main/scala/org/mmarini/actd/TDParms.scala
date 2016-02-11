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

import scala.util.Random
import breeze.linalg.DenseVector
import breeze.linalg.DenseVector
import breeze.numerics.exp
import breeze.linalg.argmax
import breeze.linalg.sum
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Bernoulli

/**
 * A set of TD parameter
 *
 * @param alpha regularization network parameter, 0 no regularization, 1 no learning
 * @param beta action update rate parameter
 * @param gamma reward discount parameter
 * @param lambda tracebilty parameter of TD(lambda) algorithm
 * @param eta learning rate parameter of neural network
 * @param maxTrainingSample maximum number of training samples
 * @param maxTrainingIteration max number of training iteration
 * @param random
 */
case class TDParms(
    alpha: Double,
    beta: Double,
    gamma: Double,
    epsilon: Double,
    lambda: Double,
    eta: Double,
    maxTrainingSamples: Int = 1,
    maxTrainingIterations: Int = 1,
    random: RandBasis) {

  private val egreedyRand = new Bernoulli(epsilon, random)

  /** Returns a [[TDParms]] with changed eta value */
  def setEta(value: Double): TDParms =
    TDParms(alpha = alpha,
      beta = beta,
      gamma = gamma,
      epsilon = epsilon,
      lambda = lambda,
      eta = value,
      maxTrainingSamples = maxTrainingSamples,
      maxTrainingIterations = maxTrainingIterations,
      random = random)

  /** Returns a index with uniform distribution with epsilon probability otherwise by softmax algorithm */
  def indexEGreedyBySoftmax(pref: DenseVector[Double]): Int =
    if (egreedyRand.sample) {
      random.randInt(pref.length).sample
    } else {
      indexByWeights(exp(pref))
    }

  /** Returns a random index with by softmax algorithm */
  def indexBySoftmax(pref: DenseVector[Double]): Int =
    indexByWeights(exp(pref))

  /** Returns a random index by weights */
  def indexByWeights(pref: DenseVector[Double]): Int = {
    val acc = pref.toArray.foldLeft(List(0.0)) {
      case (r, x) => (x + r.head) :: r
    }
    val x = random.uniform.sample * sum(pref)
    acc.reverse.tail.indexWhere(x < _)
  }

}
