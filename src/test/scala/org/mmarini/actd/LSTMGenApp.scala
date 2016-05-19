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

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.linalg.csvwrite
import breeze.stats.distributions.Rand

/**
 *
 */
object LSTMGenApp extends App with LazyLogging {
  private val TestCount = 10
  private val TestFile = "./data/lstm/test/%s/data-%d.csv"

  private val TrainCount = TestCount * 2
  private val TrainFile = "./data/lstm/train/%s/data-%d.csv"

  private val InputLayer = 3
  private val OutputLayer = 2

  object Actions extends Enumeration {
    val A0, A1 = Value
  }
  import Actions._

  object States extends Enumeration {
    val S0, S1, S2, S3, END = Value
  }
  import States._

  private val outRand = Rand.uniform

  private val Transitions: (States.Value, Actions.Value) => (States.Value, DenseVector[Double]) = {
    val map = Map(
      (S0, A0) -> (S0, DenseVector.rand[Double](OutputLayer, outRand)),
      (S0, A1) -> (S1, DenseVector.rand[Double](OutputLayer, outRand)),
      (S1, A0) -> (S1, DenseVector.rand[Double](OutputLayer, outRand)),
      (S1, A1) -> (S2, DenseVector.rand[Double](OutputLayer, outRand)),
      (S2, A0) -> (S2, DenseVector.rand[Double](OutputLayer, outRand)),
      (S2, A1) -> (S3, DenseVector.rand[Double](OutputLayer, outRand)),
      (S3, A0) -> (S3, DenseVector.rand[Double](OutputLayer, outRand)),
      (S3, A1) -> (END, DenseVector.rand[Double](OutputLayer, outRand)))
    (s: States.Value, a: Actions.Value) => map(s, a)
  }

  val StateVector: States.Value => DenseVector[Double] = Map(
    S0 -> DenseVector(-1.0, -1.0),
    S1 -> DenseVector(-1.0, 1.0),
    S2 -> DenseVector(1.0, -1.0),
    S3 -> DenseVector(1.0, 1.0))

  val ActionVector: Actions.Value => DenseVector[Double] = Map(
    A0 -> DenseVector(-1.0),
    A1 -> DenseVector(1.0))

  /** */
  private def toMatrix(in: Seq[DenseVector[Double]], colCount: Int): DenseMatrix[Double] = {
    val x = in.map(x => x.toArray)
    val y = x.flatten
    DenseMatrix.create(colCount, in.length, y.toArray).t
  }

  private val randBasis = Rand

  private val randoms: States.Value => Rand[Actions.Value] = Map(
    S0 -> randBasis.choose(Actions.values),
    S1 -> randBasis.choose(Actions.values),
    S2 -> randBasis.choose(Actions.values),
    S3 -> randBasis.choose(Actions.values))

  private def chooseAction(s0: States.Value): Actions.Value = randoms(s0).sample

  /** */
  private def createTimeSeries: (DenseMatrix[Double], DenseMatrix[Double]) = {
    def addStep(list: Seq[(DenseVector[Double], DenseVector[Double])], s0: States.Value): (Seq[(DenseVector[Double], DenseVector[Double])], States.Value) =
      if (s0 == END) {
        (list, s0)
      } else {
        val action = chooseAction(s0)
        val (s1, out) = Transitions(s0, action)
        val nextList = list :+ (DenseVector.vertcat(StateVector(s0), ActionVector(action)), out)
        addStep(nextList, s1)
      }

    val (list, _) = addStep(Seq(), S0)
    val (inList, outList) = list.unzip
    (toMatrix(inList, InputLayer), toMatrix(outList, OutputLayer))
  }

  /**  */
  private def createTimeSeries(inFile: String, outFile: String) {
    val (in, out) = createTimeSeries
    val inF = new File(inFile)
    inF.getParentFile.mkdirs
    csvwrite(inF, in)
    val outF = new File(outFile)
    outF.getParentFile.mkdirs
    csvwrite(outF, out)
  }

  /**  */
  def createDataSet(file: String, count: Int) {
    for { i <- 0 until count } {
      val inFile = file.format("in", i)
      val outFile = file.format("out", i)
      createTimeSeries(inFile, outFile)
    }
  }

  /**  */
  def createDataSet(count: Int): Seq[(DenseMatrix[Double], DenseMatrix[Double])] =
    for { i <- 0 until count } yield createTimeSeries

  createDataSet(TrainFile, TrainCount)
  createDataSet(TestFile, TestCount)

}
