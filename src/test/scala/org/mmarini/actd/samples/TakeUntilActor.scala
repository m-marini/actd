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

import org.mmarini.actd.EnvironmentActor.Interact
import org.mmarini.actd.EnvironmentActor.Step
import org.mmarini.actd.Feedback
import org.mmarini.actd.TimerLogger
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import org.mmarini.actd.TDNeuralNet
import org.mmarini.actd.TDNeuralNetTest
import org.mmarini.actd.TDAgent

object TakeUntilActor {
  def props(
    source: ActorRef,
    proposition: (Feedback, Double, TDAgent) => Boolean): Props =
    Props(classOf[TakeUntilActor], source, proposition)
}

class TakeUntilActor(
    source: ActorRef,
    proposition: (Feedback, Double, TDAgent) => Boolean) extends Actor with ActorLogging {

  val tlog: TimerLogger = new TimerLogger(log)

  def receive: Receive = {
    case _ =>
      log.info("start")
      source ! Interact
      context.become(waitingStep(sender, Seq()))
  }

  private def waitingStep(replyTo: ActorRef,
    list: Seq[(Feedback, Double, TDAgent)]): Receive = {
    case Step(f, d, a) =>
      val l = list :+ (f, d, a)
      if (proposition(f, d, a)) {
        replyTo ! l
        context stop self
      } else {
        sender ! Interact
        context become waitingStep(replyTo, l)
      }
  }
}

