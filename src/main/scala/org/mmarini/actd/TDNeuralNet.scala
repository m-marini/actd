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
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvread
import breeze.linalg.csvwrite
import breeze.stats.distributions.RandBasis
import scalax.file.Path
import scala.util.Try

/**
 * A Temporal Difference neural network that creates the network status for a given input
 *
 * @constructor create a TD learning algorithm with given weights, trace status and parameters
 * @param layers the layers
 *
 * @author us00852
 */
case class TDNeuralNet(layers: Seq[TDLayer]) {

  /** Applies the network to an input pattern */
  def apply(in: DenseVector[Double]): TDNetStatus = {
    val (stats, _) = layers.foldLeft((Seq[TDLayerStatus](), in)) {
      case ((stats, in), layer) =>
        val status = layer(in)
        (stats :+ status, status.output)
    }
    new TDNetStatus(stats)
  }

  /** Clears the traces */
  def clearTraces: TDNeuralNet = new TDNeuralNet(layers.map(_.clearTraces))

  /** Write the weights to a filename set */
  def write(file: String) {
    val fn = s"$file-n.csv"
    Path.fromString(fn).deleteIfExists()
    csvwrite(new File(fn), DenseMatrix(layers.size.toDouble))
    for {
      (layer, i) <- layers.zipWithIndex
      fn = s"$file-$i.csv"
    } {
      Path.fromString(fn).deleteIfExists()
      csvwrite(new File(fn), layer.weights)
    }
  }
}

/**
 * A factory of [[TDNeuralNet]]
 */
object TDNeuralNet {

  /**
   * Creates a [[TDNeuralNet]] with parameters and hidden layers
   *
   * @param p parameters
   * @param layerSize size of each hidden layer
   */
  def apply(parms: TDParms)(layerSizes: Seq[Int]): TDNeuralNet = {
    require(layerSizes.size >= 2)

    val layerIO = layerSizes zip layerSizes.tail
    val hiddens = for {
      (m, n) <- layerIO.init
    } yield {
      TDLayer(n, m, TDLayerParms.hidden(parms))(parms.random)
    }
    val output = layerIO.last match {
      case (m, n) => TDLayer(n, m, TDLayerParms.nlr(parms))(parms.random)
    }
    TDNeuralNet(hiddens :+ output)
  }

  /**
   * Creates a [[TDNeuralNet]] with parameters by reading file set
   *
   * @param p parameters
   * @param file filename
   */
  def read(parms: TDParms)(file: String): TDNeuralNet = {
    val n = csvread(new File(s"$file-n.csv"))(0, 0).toInt
    val hiddens = for {
      i <- 0 until n - 1
    } yield TDLayer.read(TDLayerParms.hidden(parms))(s"$file-$i.csv")
    val output = TDLayer.read(TDLayerParms.nlr(parms))(s"$file-${n - 1}.csv")

    TDNeuralNet(hiddens :+ output)
  }
}
