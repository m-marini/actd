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
import breeze.numerics.signum
import breeze.linalg.argmax
import breeze.linalg.sum
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Bernoulli
import breeze.linalg.DenseMatrix
import breeze.linalg.operators.DenseMatrix_OrderingOps

/**
 * A set of TD layer parameter
 *
 * @param activation activation function
 * @param gradActivation the gradient of activation function
 * @param cost the cost function
 * @param gradCost the gradient of cost function
 * @param backprop the back propagation function
 * @param decay decay parameter of eligibility trace
 * @param eta learning rate parameter
 */
case class TDLayerParms(
    activation: DenseVector[Double] => DenseVector[Double],
    gradActivation: (DenseVector[Double], DenseVector[Double]) => DenseVector[Double],
    cost: (DenseVector[Double], DenseMatrix[Double]) => Double,
    gradCost: (DenseVector[Double], DenseVector[Double], DenseVector[Double], DenseMatrix[Double]) => DenseMatrix[Double],
    backprop: DenseMatrix[Double] => DenseMatrix[Double],
    decay: Double,
    eta: Double) {
}

/**
 * A factory of [[TDLayerParms]]
 */
object TDLayerParms {
  /**
   * Creates non linear regression layer parameters
   * Both activation function and back propagation function are the identity functions
   *
   * @param p the network parameter set
   */
  def nlr(p: TDParms): TDLayerParms = TDLayerParms(
    NetFunctions.identity,
    NetFunctions.gradIdentity,
    NetFunctions.elasticNet(p.l1, p.l2),
    NetFunctions.gradElasticNet(p.l1, p.l2),
    NetFunctions.matrixIdentity,
    p.lambda * p.gamma,
    p.eta)

  /**
   * Creates hidden layer parameters
   * Activation function is tanh
   * and back propagation function is signum
   * @param p the network parameter set
   */
  def hidden(p: TDParms): TDLayerParms = TDLayerParms(
    NetFunctions.tanh,
    NetFunctions.gradTanh,
    NetFunctions.elasticNet(p.l1, p.l2),
    NetFunctions.gradElasticNet(p.l1, p.l2),
    NetFunctions.signum,
    p.lambda * p.gamma,
    p.eta)
}
