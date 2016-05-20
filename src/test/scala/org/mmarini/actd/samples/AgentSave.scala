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

package org.mmarini.actd.samples

import scala.concurrent.duration.FiniteDuration

import org.mmarini.actd.TDAgentActor.CurrentAgent
import com.typesafe.scalalogging.LazyLogging
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Terminated
import scala.concurrent.duration.DurationInt
import akka.actor.Props
import org.mmarini.actd.TDAgentActor.QueryAgent
import org.mmarini.actd.ACAgent
import org.mmarini.actd.ACAgent
import org.mmarini.actd.Agent
import org.mmarini.actd.ACAgent

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
trait AgentSave extends WallEnvironment with LazyLogging {

  val agentFilename: String = "docs/agent"
  val trainTime = 10 seconds

  def controllerActor: ActorRef

  lazy val saveActors: Seq[ActorRef] = {
    val consumer = system.actorOf(ConsumerActor.props({
      case x =>
    }))
    def save(agent: Agent) {
      logger.info(s"Save agent in $agentFilename")
      agent.write(agentFilename)
    }
    val saveActor = system.actorOf(Props(classOf[AgentSaveActor], environment, consumer, trainTime, save _))
    Seq(consumer, saveActor)
  }
}

class AgentSaveActor(environment: ActorRef, controller: ActorRef, time: FiniteDuration, onSuccess: (Agent) => Unit) extends Actor with ActorLogging {

  context watch controller

  def receive: Receive = {
    case Terminated(`controller`) =>
      log.info("Waiting for training")
      context.system.scheduler.scheduleOnce(time, environment, QueryAgent)(context.system.dispatcher, self)

    case CurrentAgent(agent) =>
      onSuccess(agent)
      context stop self
  }
}
