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

import scala.math.sqrt

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import breeze.stats.distributions.Rand
import breeze.stats.distributions.RandBasis

/**
 * A Temporal Difference network layer that creates the layer status for a given input.
 *
 * @constructor create a TD learning algorithm with given weights, trace status and parameters
 * @param weights the weight
 * @param trace the trace status
 * @param parms the parameters
 *
 * @author us00852
 */
case class TDLayer(weights: DenseMatrix[Double], traces: DenseMatrix[Double], parms: TDLayerParms) {
  /** Creates the layer status by a given input */
  def apply(in: DenseVector[Double]): TDLayerStatus = new TDLayerStatus(in, this)

  /** Clears the eligibility traces */
  def clearTraces: TDLayer = TDLayer(weights, traces * 0.0, parms)
}

/**
 * A factory of [[TDLayer]]
 */
object TDLayer {

  /**
   * Creates a TDLayer with input nodes, output nodes and layer parameters
   * @param n the number of output node
   * @param m the number of input node
   * @param parms the parameters
   * @param randB the the randomizer basis
   */
  def apply(n: Int, m: Int, parms: TDLayerParms)(implicit randB: RandBasis = Rand): TDLayer = {
    val v = 2.0 / (n + m + 1)
    val weights = DenseMatrix.rand(n, m + 1, randB.gaussian(0.0, sqrt(v)))
    TDLayer(weights, DenseMatrix.zeros[Double](n, m + 1), parms)
  }

  /**
   * Creates a TDLayer with layer parameters by reading file set
   * @param parms the parameters
   * @param file the filename
   */
  def read(parms: TDLayerParms)(file: String): TDLayer = {
    val weights = csvread(new File(file))
    val n = weights.rows
    val m = weights.cols
    TDLayer(weights, DenseMatrix.zeros[Double](n, m), parms)
  }

}
