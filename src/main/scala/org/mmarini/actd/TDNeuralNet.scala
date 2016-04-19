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

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.numerics.sigmoid

/**
 * A Temporal Difference learning algorithms that creates the network status for a given input and
 * creates new [[TDNeuralNet]] by learning from an input vector and and expected output vector.
 *
 * @constructor create a TD learning algorithm with given weights, trace status and parameters
 * @param weights the weight
 * @param trace the trace status
 * @param parms the parameters
 *
 * @author us00852
 */
class TDNeuralNet(
    val weights: MatrixSeq,
    val trace: MatrixSeq,
    val parms: TDParms) extends NeuralNet {

  /** Creates a new [[TDLearning]] by a given input and an expected output */
  def learn(in: DenseVector[Double], expected: DenseVector[Double]): TDNeuralNet = {
    val status = apply(in)
    val error = expected - status.output
    val grad = status.gradient(error, parms.alpha)
    val t1 = trace * (parms.gamma * parms.lambda) - grad
    val w1 = weights + t1 * parms.eta
    val cost0 = status.cost(expected, parms.alpha)
    val cost1 = nlr(in, w1).cost(expected, parms.alpha)
    if (cost1 < cost0) {
      new TDNeuralNet(w1, t1, parms)
    } else {
      new TDNeuralNet(weights, trace, parms.setEta(parms.eta / 2))
    }
  }

  /** Applies the network to an input pattern */
  def apply(in: DenseVector[Double]): NetStatus = nlr(in, weights)

  /** Clears traces */
  def clearTraces: TDNeuralNet =
    new TDNeuralNet(weights, trace.zeros, parms)

  /**
   * Computes the status of a Non Linear Regression network
   *
   * @param in the input
   * @param weights the weights of each layer
   */
  private def nlr(in: DenseVector[Double],
    weights: MatrixSeq): NetStatus = {
    val in1 = DenseVector.vertcat(DenseVector.ones[Double](1), in).toDenseMatrix.t
    val s = weights.matrices.init.foldLeft(Seq(in1))((res, w) => {
      val in = res.head
      val z = w * in
      val h = sigmoid(z)
      val h1 = DenseMatrix.vertcat(DenseMatrix.ones[Double](1, 1), h)
      h1 +: res
    })
    val out = DenseMatrix.vertcat(DenseMatrix.ones[Double](1, 1), weights.matrices.last * s.head)
    new NLRStatus(weights, (out +: s).map(_.toDenseVector).reverse)
  }

}

/**
 * a Factory of [[TDNeuralNet]] instances.
 */
object TDNeuralNet {

  /** Creates a [[TDLearning]] with number of neuron layers, parameters and sigma random weights initialization s*/
  def apply(layers: Seq[Int], parms: TDParms): TDNeuralNet =
    new TDNeuralNet(MatrixSeq.createXavier(layers)(parms.random), MatrixSeq.zeros(layers), parms)
}
