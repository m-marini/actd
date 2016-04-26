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
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand
import scala.math.sqrt

case class TDLayer(weights: DenseMatrix[Double], traces: DenseMatrix[Double], parms: TDLayerParms) {
  def apply(in: DenseVector[Double]): TDLayerStatus = {
    val in1 = DenseVector.vertcat(DenseVector.ones[Double](1), in)
    val z = weights * in1
    val h = parms.activation(z)
    new TDLayerStatus(h, in, this)
  }

  def clearTraces: TDLayer = TDLayer(weights, traces * 0.0, parms)

}

object TDLayer {
  
  def apply(n: Int, m: Int, parms: TDLayerParms)(implicit randB: RandBasis = Rand): TDLayer = {
    val v = 2.0 / (n + m + 1)
    val weights = DenseMatrix.rand(n, m + 1, randB.gaussian(0.0, sqrt(v)))
    TDLayer(weights, DenseMatrix.zeros[Double](n, m + 1), parms)
  }
}
