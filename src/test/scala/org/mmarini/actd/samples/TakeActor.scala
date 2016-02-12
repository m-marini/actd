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
import org.mmarini.actd.Feedback
import org.mmarini.actd.TimerLogger
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Props
import akka.actor.actorRef2Scala
import org.mmarini.actd.EnvironmentActor.Step
import akka.actor.Terminated
import akka.actor.ActorRef

object TakeActor {
  def props(
    source: ActorRef,
    target: ActorRef,
    count: Int): Props =
    Props(classOf[TakeActor], source, target, count)
}

class TakeActor(
    source: ActorRef,
    target: ActorRef,
    count: Int) extends Actor with ActorLogging {
  var counter = count
  var list: Seq[(Feedback, Double)] = Seq()

  val tlog = new TimerLogger(log)

  context watch source
  context watch target

  source ! Interact

  def receive: Receive = {
    case Terminated(`source`) =>
      context stop target
      context stop self
    case Terminated(`target`) =>
      context stop source
      context stop self
    case Step(f, d) =>
      list = list :+ (f.asInstanceOf[Feedback], d.asInstanceOf[Double])
      tlog.info(s"counter = $counter")
      counter = counter - 1
      if (counter > 0) {
        sender ! Interact
      } else {
        log.info(s"${list.length} size")
        target ! list
        context stop source
        context stop self
      }

  }
}
