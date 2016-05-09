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

import org.deeplearning4j.nn.conf.NeuralNetConfiguration
import org.deeplearning4j.nn.api.OptimizationAlgorithm
import org.deeplearning4j.nn.weights.WeightInit
import org.deeplearning4j.nn.conf.Updater
import org.deeplearning4j.nn.conf.layers.GravesLSTM
import org.nd4j.linalg.lossfunctions.LossFunctions.LossFunction
import org.deeplearning4j.nn.conf.layers.RnnOutputLayer
import org.deeplearning4j.nn.conf.BackpropType
import org.deeplearning4j.nn.multilayer.MultiLayerNetwork
import org.deeplearning4j.optimize.listeners.ScoreIterationListener
import com.typesafe.scalalogging.LazyLogging
import org.nd4j.linalg.api.ndarray.INDArray
import org.nd4j.linalg.factory.Nd4j
import org.nd4j.linalg.indexing.NDArrayIndex
import org.nd4j.linalg.lossfunctions.LossFunctions

object LSTMTestApp extends App with LazyLogging {
  private val InputLayer = 2
  private val HiddenLayer = 2
  private val OutputLayer = 2
  private val Seed = 12345
  private val TBPTTLength = 4

  val conf = new NeuralNetConfiguration.Builder().
    optimizationAlgo(OptimizationAlgorithm.STOCHASTIC_GRADIENT_DESCENT).
    iterations(1).
    learningRate(0.1).
    rmsDecay(0.95).
    seed(Seed).
    regularization(true).
    l2(1e-6).
    weightInit(WeightInit.XAVIER).
    updater(Updater.RMSPROP).
    list(2).
    layer(0, new GravesLSTM.Builder().
      nIn(InputLayer).
      nOut(HiddenLayer).
      activation("tanh").
      build()).
    layer(1, new RnnOutputLayer.Builder().
      lossFunction(LossFunctions.LossFunction.RMSE_XENT).
      activation("identity").
      nIn(HiddenLayer).
      nOut(OutputLayer).
      build()).
    backpropType(BackpropType.TruncatedBPTT).
    tBPTTForwardLength(TBPTTLength).
    tBPTTBackwardLength(TBPTTLength).
    pretrain(false).
    backprop(true).
    build()

  val net = new MultiLayerNetwork(conf)
  net.init()
  logger.info("Network init")
  net.setListeners(new ScoreIterationListener(1))

  val (in, out) = loadTrainSet

  for { i <- 1 to 100 } {
    net.fit(in, out)
  }

  net.rnnClearPreviousState();
  for { i <- 0 until in.size(2) } {
    val output = net.rnnTimeStep(in.get(NDArrayIndex.all(), NDArrayIndex.all(), NDArrayIndex.point(0)));
    logger.info(s"out = ${out.get(NDArrayIndex.all(), NDArrayIndex.all(), NDArrayIndex.point(0))}")
    logger.info(s"output = ${output}")
  }
  logger.info("Completed")

  private def loadTrainSet: (INDArray, INDArray) = {
    val in = Nd4j.zeros(3, 2, 4)
    val out = Nd4j.zeros(3, 2, 4)
    for {
      i <- 1 to 3
      j <- 1 to 2
      k <- 1 to 4
    } {
      in.putScalar(Array(i - 1, j - 1, k - 1), i * 100 + j * 10 + k)
      out.putScalar(Array(i - 1, j - 1, k - 1), i + j * 10 + k * 100)
    }
    logger.info(s"in = ${in}")
    logger.info(s"out = ${out}")

    (in, out)
  }
}
