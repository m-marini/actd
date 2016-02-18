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

/**
 * A trainer that changes the networks to fit a sample set
 *
 * @constructor create a trainer for a initial network and training samples
 * @parm net the network
 * @parm samples the samples
 * @parm maxSize
 *
 * @author us00852
 */
case class TDTrainer(maxSamples: Int = 1, samples: Seq[Feedback] = Seq()) {

  /** Creates a new trainer with samples fed by a new feedback */
  def feed(f: Feedback): TDTrainer =
    TDTrainer(
      maxSamples = maxSamples,
      samples = (samples :+ f) takeRight maxSamples)

  /** Creates a new trainer with samples fed by a new feedback */
  def train(net: TDNeuralNet): TDNeuralNet = {

    /** Returns a new [[TDNeuralNet]] by a [[TDNeuralNet]] with the a single feedback */
    def trainSample(net: TDNeuralNet, feedback: Feedback): TDNeuralNet = {

      // Computes the state value pre and post step
      val s0Vect = feedback.s0.toDenseVector
      val s1Vect = feedback.s1.toDenseVector

      val end0 = feedback.s0.finalStatus
      val end1 = feedback.s1.finalStatus

      // The status value of post state is 0 if final episode else bootstraps from critic
      val postValue = if (end1 || end0) 0.0 else net(s1Vect).output(0)

      // Computes the expected state value by booting the previous status value */
      val expectedValue = postValue * net.parms.gamma + feedback.reward

      // Computes the error by critic
      val preValue = net(s0Vect).output(0)

      // Trains the critic by evidence
      net.learn(s0Vect, DenseVector(expectedValue))
    }

    samples.foldLeft(net)(trainSample)
  }
}
