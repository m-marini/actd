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

import TrainerActor.Trained
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import akka.actor.actorRef2Scala
import breeze.linalg.DenseVector

/** Props and messages factory for TrainerActor */
object TrainerActor {

  /** Props for TrainerActor */
  def props: Props = Props(classOf[TrainerActor])

  /** Message sent by [[TrainerActor]] when a [[TDNeuralNet]] has been trained */
  case class Trained(net: TDNeuralNet)

  /** Message sent to [[TrainerActor]] to train a [[TDNeuralNet]] */
  case class Train(net: TDNeuralNet)

  /** Message sent to [[TrainerActor]] to set the training set */
  case class TrainSet(feedbacks: Seq[Feedback])
}

/**
 * An actor that trains a critic neural network with a sequence of feedbacks.
 *
 * It produces the critic after a complete cycle of training.
 */
class TrainerActor extends Actor with ActorLogging {

  import TrainerActor._

  var feedbacks: Seq[Feedback] = Seq()

  /** Returns a new [[TDNeuralNet]] by a [[TDNeuralNet]] with the current feedbacks */
  private def train(net: TDNeuralNet): TDNeuralNet = {

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

      // Teaches the critic by evidence
      net.learn(s0Vect, DenseVector(expectedValue))
    }

    feedbacks.foldLeft(net)(trainSample)
  }

  def receive: Receive = {
    case TrainSet(feedbacks) => this.feedbacks = feedbacks

    case Train(net: TDNeuralNet) =>
      val nn = train(net)
      sender ! Trained(nn)
  }
}
