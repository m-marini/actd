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

import breeze.linalg.{ * => * }
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import com.typesafe.scalalogging.LazyLogging

/**
 * A status of Non Linear Regression neural network.
 * .
 * @constructor create a status with weights and layers values
 * @param weights the synapse weights
 * @param layers the neuro state
 *
 * @author us00852
 */
class NLRStatus(
    val weights: MatrixSeq,
    val layers: Seq[DenseVector[Double]]) extends NetStatus {

  /** Computes the back propagation tensor */
  def bTensor: MatrixSeq = {
    val bt = for {
      (h, w) <- layers.tail.init zip weights.matrices.tail
    } yield {
      val n = h.length
      val hh = h(1 to -1)
      val dz = hh :* (DenseVector.ones[Double](n - 1) - hh)
      val w0 = w(::, 1 to -1)
      val b = w0(*, ::) :* dz
      b.t
    }
    MatrixSeq(bt)
  }

}
