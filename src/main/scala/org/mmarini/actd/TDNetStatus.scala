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
import breeze.linalg.operators.DenseVector_GenericOps

class TDNetStatus(stats: Seq[TDLayerStatus]) {

  def output = stats.last.output

  def train(delta: DenseVector[Double]): (TDNeuralNet1, Double) = {
    // Computes the trained layers
    val (layers, _) = stats.foldRight((Seq[TDLayer](), delta)) {
      case (status, (layers, delta)) =>
        val (layer, delta1) = status.train(delta)
        (layers :+ layer, delta1)
    }
    // Computes the cost on output layer
    val y = stats.last.cost(delta)
    // Fold all previous layer cost
    val yy = for {
      layer <- stats.init
    } yield {
      layer.cost(DenseVector.zeros(layer.output.size))
    }
    (new TDNeuralNet1(layers), y + yy.sum)
  }

}