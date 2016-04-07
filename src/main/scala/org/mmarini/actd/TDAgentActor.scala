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
import akka.actor.ActorRef

/** Props and messages factory for [[TDAgentActor]] */
object TDAgentActor {

  /**
   * Creates the props for a [[TDAgentActor]]
   *
   * @param agent initial agent
   */
  def props(agent: TDAgent): Props = Props(classOf[TDAgentActor], agent)

  /** Message to [[TDAgentActor]] to process a single step interaction */
  object QueryAgent

  /** Message sent to [[TDAgentActor]] to react with an action */
  case class React(status: Status)

  /** Message sent to [[TDAgentActor]] to reply to a QueryAgent */
  case class CurrentAgent(agent: TDAgent)

  /** Message sent by [[TDAgentActor]] to reply a [[React]] */
  case class Reaction(action: Action)

  /** Message sent to [[TDAgentActor]] to add a feedback */
  case class Train(feedback: Feedback)

  /** Message sent by [[TDAgentActor]] to reply a [[Feed]] */
  case class Trained(delta: Double, agent: TDAgent)
}

/**
 * An Actor that plays the agent role in the environment agent interaction
 *
 * @constructor create the actor
 * @param agent the initial agent
 */
class TDAgentActor(agent: TDAgent) extends Actor with ActorLogging {

  import TDAgentActor._

  val trainerActorOpt: Option[ActorRef] = if (agent.parms.maxTrainingSamples > 0) {
    Some(context.actorOf(TrainerActor.props))
  } else {
    None
  }

  def receive: Receive = waitingFirstFeed(agent)

  /** Processes messages before trainer actor started */
  private def waitingFirstFeed(agent: TDAgent): Receive = {

    case React(s) =>
      sender ! Reaction(agent.action(s))

    case QueryAgent =>
      sender ! CurrentAgent(agent)

    case Train(feedback) =>
      val (na, delta) = agent.train(feedback)
      val trainer = TDTrainer(agent.parms.maxTrainingSamples, Seq(feedback))
      for (trainerActor <- trainerActorOpt) { trainerActor ! TrainerActor.Train(na.critic, trainer) }
      context become processing(na, trainer)
      sender ! Trained(delta, na)
  }

  /** Processes messages after the trainer actor has started */
  private def processing(agent: TDAgent, trainer: TDTrainer): Receive = {

    case TrainerActor.Trained(net) =>
      context become processing(agent.critic(net), trainer)
      for (trainerActor <- trainerActorOpt) { trainerActor ! TrainerActor.Train(net, trainer) }

    case React(s) =>
      sender ! Reaction(agent.action(s))

    case QueryAgent =>
      sender ! CurrentAgent(agent)

    case Train(feedback) =>
      val (na, delta) = agent.train(feedback)
      context become processing(na, trainer.feed(feedback))
      sender ! Trained(delta, na)
  }
}
