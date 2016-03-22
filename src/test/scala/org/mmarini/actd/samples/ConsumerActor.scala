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

import akka.actor.Props
import akka.actor.Actor.Receive
import akka.actor.Terminated
import akka.actor.ActorLogging
import akka.actor.Actor
import akka.actor.ActorRef

object ConsumerActor {

  def props(consume: Receive): Props = Props(classOf[ConsumerActor], consume)

}

class ConsumerActor(consume: Receive) extends Actor with ActorLogging {

  def receive: Receive = waitingStep(Set())

  private def waitingStep(sources: Set[ActorRef]): Receive = {

    case Terminated(source) =>
      val reminder = sources - source
      if (reminder.isEmpty) {
        log.info("Completed ConsumerActor")
        context stop self
      } else {
        context become waitingStep(reminder)
      }

    case msg =>
      consume(msg)
      if (!sources.contains(sender)) {
        log.info(s"Watching $sender")
        context watch sender
        context become waitingStep(sources + sender)
      }
  }
}
