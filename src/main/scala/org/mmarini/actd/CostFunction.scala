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

import breeze.numerics.sigmoid
import breeze.numerics.tanh
import breeze.numerics.cosh
import breeze.numerics.signum
import breeze.numerics.abs
import breeze.linalg.DenseMatrix
import breeze.linalg.sum

trait CostFunction {
  def apply(delta: DenseVector[Double], w: DenseMatrix[Double]): Double

  def grad(delta: DenseVector[Double], grad: DenseVector[Double], x: DenseVector[Double], w: DenseMatrix[Double]): DenseMatrix[Double]
}

object CostFunction {
  def elasticNet(l1: Double, l2: Double): CostFunction = new CostFunction() {

    /**
     * Cost(delta, w) = delta^2 + l2* sum(w)) / 2 + l1 * sum(abs(w2))
     */
    def apply(delta: DenseVector[Double], w: DenseMatrix[Double]): Double = {
      val w1 = w(::, 1 to -1)
      val w2 = w1 :* w1
      (delta.t * delta + l2 * sum(w2)) / 2 + l1 * sum(abs(w1))
    }

    /**
     * Grad(delta, f', x, w) = -(1-alpha) * delta * f' * x + l2 * w + l1 * signum(w)
     */
    def grad(delta: DenseVector[Double], grad: DenseVector[Double], x: DenseVector[Double], w: DenseMatrix[Double]): DenseMatrix[Double] = {
      val k = delta :* grad
      val x1 = DenseVector.vertcat(DenseVector.ones[Double](1), x)
      val e = k * x1.t
      val n = w.rows
      val gw = DenseMatrix.horzcat(DenseMatrix.zeros[Double](n, 1), w(::, 1 to -1))
      l2 * gw + l1 * signum(gw) - e
    }
  }
}
