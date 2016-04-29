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

trait ActivationFunction {
  def apply(in: DenseVector[Double]): DenseVector[Double]

  def grad(out: DenseVector[Double], in: DenseVector[Double]): DenseVector[Double]
}

object Ident extends ActivationFunction {
  def apply(in: DenseVector[Double]): DenseVector[Double] = in

  def grad(out: DenseVector[Double], in: DenseVector[Double]): DenseVector[Double] = DenseVector.ones(out.length)
}

object Sigmoid extends ActivationFunction {
  def apply(in: DenseVector[Double]): DenseVector[Double] = sigmoid(in)

  def grad(out: DenseVector[Double], in: DenseVector[Double]): DenseVector[Double] = (1.0 - out) :* out
}

object Tanh extends ActivationFunction {
  def apply(in: DenseVector[Double]): DenseVector[Double] = tanh(in)

  def grad(out: DenseVector[Double], in: DenseVector[Double]): DenseVector[Double] = (1.0 - out) :* (out + 1.0)
}
