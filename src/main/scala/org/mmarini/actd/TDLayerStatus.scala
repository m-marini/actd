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
import breeze.linalg.operators.DenseMatrix_OrderingOps

case class TDLayerStatus(output: DenseVector[Double], input: DenseVector[Double], layer: TDLayer) {

  def train(delta: DenseVector[Double]): (TDLayer, DenseVector[Double], Double) = {
    val grad = layer.parms.activation.grad(output, input)
    val grad1 = layer.parms.cost.grad(delta, grad, input, layer.weights)
    val trace = layer.parms.traceFunc(layer.traces, grad1)
    val weights = layer.weights + layer.parms.eta * trace
    val delta1 = layer.weights(::, 1 to -1).t * (delta :* grad)
    val layer1 = TDLayer(weights, trace, layer.parms)
    val cost = layer.parms.cost.apply(delta, layer.weights)
    (layer1, delta1, cost)
  }
}