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
import breeze.linalg.InjectNumericOps
import breeze.linalg.sum
import breeze.numerics.abs

/** The identity function */
object NetFunctions {

  /** Returns y = x */
  def matrixIdentity(x: DenseMatrix[Double]): DenseMatrix[Double] = x

  /** Returns y = x */
  def signum(x: DenseMatrix[Double]): DenseMatrix[Double] = breeze.numerics.signum(x)

  /** Returns y = x */
  def identity(x: DenseVector[Double]): DenseVector[Double] = x

  /** Returns the gradient y' = 1 */
  def gradIdentity(y: DenseVector[Double], x: DenseVector[Double]): DenseVector[Double] =
    DenseVector.ones(y.length)

  /** Returns y = 1 / (1 + exp(-x)) */
  def sigmoid(x: DenseVector[Double]): DenseVector[Double] = breeze.numerics.sigmoid(x)

  /** Returns the gradient y' = y  (1 - y) */
  def gradSigmoid(y: DenseVector[Double], x: DenseVector[Double]): DenseVector[Double] = (1.0 - y) :* y

  /** Returns y = tanh(x) */
  def tanh(x: DenseVector[Double]): DenseVector[Double] = breeze.numerics.tanh(x)

  /** Returns the gradient y' = (1 + y)  (1 - y) */
  def gradTanh(y: DenseVector[Double], x: DenseVector[Double]): DenseVector[Double] = (1.0 - y) :* (y + 1.0)

  /**
   * Returns cost(delta, w) = delta^2 + l2* sum(w)) / 2 + l1 * sum(abs(w2))
   */
  def elasticNet(l1: Double, l2: Double)(delta: DenseVector[Double], w: DenseMatrix[Double]): Double = {
    val w1 = w(::, 1 to -1)
    val w2 = w1 :* w1
    (delta.t * delta + l2 * sum(w2)) / 2 + l1 * sum(abs(w1))
  }

  /**
   * Grad(delta, f', x, w) = -(1-alpha) * delta * f' * x + l2 * w + l1 * signum(w)
   */
  def gradElasticNet(l1: Double,
    l2: Double)(delta: DenseVector[Double],
      grad: DenseVector[Double],
      x: DenseVector[Double],
      w: DenseMatrix[Double]): DenseMatrix[Double] = {
    val k = delta :* grad
    val x1 = DenseVector.vertcat(DenseVector.ones[Double](1), x)
    val e = k * x1.t
    val n = w.rows
    val gw = DenseMatrix.horzcat(DenseMatrix.zeros[Double](n, 1), w(::, 1 to -1))
    l2 * gw + l1 * signum(gw) - e
  }
}
