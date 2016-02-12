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

import TDAgentActor.Reaction
import TDAgentActor.Trained
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import akka.actor.actorRef2Scala
import breeze.linalg.DenseVector
import org.mmarini.actd.TrainerActor.Train
import org.mmarini.actd.TrainerActor.TrainSet

/** Props and messages factory for [[TDAgentActor]] */
object TDAgentActor {

  /**
   * Creates the props for a [[TDAgentActor]]
   *
   * @param parms parameters
   * @param critic the initial critic network
   * @param actor the initial actor network
   */
  def props(parms: TDParms,
    critic: TDNeuralNet,
    actor: TDNeuralNet): Props =
    Props(classOf[TDAgentActor], parms, critic, actor)

  /** Message sent to [[TDAgentActor]] to react with an action */
  case class React(status: Status)

  /** Message sent by [[TDAgentActor]] to reply a [[React]] */
  case class Reaction(action: Action)

  /** Message sent to [[TDAgentActor]] to add a feedback */
  case class Feed(feedback: Feedback)

  /** Message sent by [[TDAgentActor]] to reply a [[Feed]] */
  case class Trained(delta: Double)
}

/**
 * An Actor that plays the agent role in the environment agent interaction
 *
 * @constructor create the agent
 * @param parms parameters
 * @param critic the initial critic network
 * @param actor the initial actor network
 */
class TDAgentActor(parms: TDParms,
    critic: TDNeuralNet,
    actor: TDNeuralNet) extends Actor with ActorLogging {

  import TDAgentActor._

  var currentCritic = critic
  var currentActor = actor
  var feedbacks: Seq[Feedback] = Seq()

  val trainer = context.actorOf(TrainerActor.props)

  def receive: Receive = {
    case TrainerActor.Trained(net) =>
      currentCritic = net
      trainer ! Train(net)

    case React(status) =>
      val action = parms.indexEGreedyBySoftmax(currentActor(status.toDenseVector).output)
      sender ! Reaction(action)

    case Feed(feedback) =>
      val nf = (feedbacks :+ feedback).takeRight(parms.maxTrainingSamples)
      trainer ! TrainSet(nf)
      if (feedbacks.isEmpty) trainer ! TrainerActor.Train(critic)
      feedbacks = nf

      val delta = train(feedback)

      sender ! Trained(delta)
  }

  /** Returns a new agent that has learned by reward and the error */
  private def train(feedback: Feedback): Double = {

    // Computes the state value pre and post step
    val s0Vect = feedback.s0.toDenseVector
    val s1Vect = feedback.s1.toDenseVector

    val end0 = feedback.s0.finalStatus
    val end1 = feedback.s1.finalStatus

    // The status value of post state is 0 if final episode else bootstraps from critic
    val postValue = if (end1 || end0) 0.0 else critic(s1Vect).output(0)

    // Computes the expected state value by booting the previous status value */
    val expectedValue = postValue * parms.gamma + feedback.reward

    // Computes the error by critic
    val preValue = critic(s0Vect).output(0)
    val delta = expectedValue - preValue

    // Teaches the critic by evidence
    val nc = currentCritic.learn(s0Vect, DenseVector(expectedValue))

    // Computes the expected action preferences applying the critic error to previous decision */
    val pref = actor(s0Vect).output
    val expectedPref = pref.copy
    val action = feedback.action
    expectedPref(action to action) += parms.beta * delta

    // Teaches the actor by evidence
    val na = actor.learn(s0Vect, expectedPref)

    currentCritic = if (end0) nc.clearTraces else nc
    currentActor = if (end0) na.clearTraces else na
    delta
  }

}
