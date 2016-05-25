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

import org.apache.commons.math3.random.MersenneTwister

import breeze.linalg.DenseVector
import breeze.linalg.csvread
import breeze.linalg.csvwrite
import breeze.linalg.max
import breeze.linalg.sum
import breeze.numerics.exp
import breeze.stats.distributions.Bernoulli
import breeze.stats.distributions.RandBasis

/**
 * A set of TD parameter
 *
 * @param beta action update rate parameter
 * @param gamma reward discount parameter
 * @param lambda eligibility parameter of TD(lambda) algorithm
 * @param eta learning rate parameter of neural network
 * @param l1 regularization network parameter abs(w), 0 no regularization
 * @param l2 regularization network parameter w^2, 0 no regularization
 * @param random
 */
case class TDParms(
    beta: Double,
    gamma: Double,
    epsilon: Double,
    lambda: Double,
    eta: Double,
    l1: Double,
    l2: Double,
    decayEta: Double,
    random: RandBasis) {

  private val epsilonRand = new Bernoulli(epsilon, random)

  /** Returns a [[TDParms]] with decayed eta value */
  def decay: TDParms =
    TDParms(beta = beta,
      gamma = gamma,
      epsilon = epsilon,
      lambda = lambda,
      eta = eta * decayEta,
      l1 = l1,
      l2 = l2,
      decayEta = decayEta,
      random = random)

  /** Returns a [[TDParms]] with changed eta value */
  def setEta(value: Double): TDParms =
    TDParms(beta = beta,
      gamma = gamma,
      epsilon = epsilon,
      lambda = lambda,
      eta = value,
      l1 = l1,
      l2 = l2,
      decayEta = decayEta,
      random = random)

  def chooseEpsilon(f: => Int, choice: Int): Int =
    if (epsilonRand.sample) random.randInt(choice).sample else f

  /** Returns a index with uniform distribution with epsilon probability otherwise by softmax algorithm */
  def chooseESoftmax(pref: DenseVector[Double]): Int = chooseEpsilon(chooseSoftmax(pref), pref.length)

  /** Returns a index with uniform distribution with epsilon probability otherwise by greedy algorithm */
  def chooseGreedy(pref: DenseVector[Double]): Int = {
    val mx = max(pref)
    pref.findAll(_ == mx)(0)
  }

  /** Returns a index with uniform distribution with epsilon probability otherwise by greedy algorithm */
  def chooseEGreedy(pref: DenseVector[Double]): Int = chooseEpsilon(chooseGreedy(pref), pref.length)

  /** Returns a random index with by softmax algorithm */
  def chooseSoftmax(pref: DenseVector[Double]): Int = chooseWeights(exp(pref))

  /** Returns a random index by weights */
  def chooseWeights(pref: DenseVector[Double]): Int = {
    val acc = pref.toArray.foldLeft(List(0.0)) {
      case (r, x) => (x + r.head) :: r
    }
    val x = random.uniform.sample * sum(pref)
    val i = acc.init.reverse.indexWhere(x < _)
    if (i >= 0) i else pref.length - 1
  }

  def toDenseVector: DenseVector[Double] = DenseVector[Double](
    beta,
    gamma,
    epsilon,
    lambda,
    eta,
    l1,
    l2,
    decayEta)

  def write(file: String) {
    csvwrite(new File(file), toDenseVector.toDenseMatrix)
  }
}

object TDParms {

  private val LambdaIndex = 3
  private val EtaIndex = 4
  private val L1Index = 5
  private val L2Index = 6
  private val DecayEtaIndex = 7
  //  private val MaxTrainingSamplesIndex = 6

  def apply(file: String, random: RandBasis = new RandBasis(new MersenneTwister())): TDParms = {
    val p = csvread(new File(file))
    TDParms(
      beta = p(0, 0),
      gamma = p(0, 1),
      epsilon = p(0, 2),
      lambda = p(0, LambdaIndex),
      eta = p(0, EtaIndex),
      l1 = p(0, L1Index),
      l2 = p(0, L2Index),
      decayEta = p(0, DecayEtaIndex),
      random)
  }
}
