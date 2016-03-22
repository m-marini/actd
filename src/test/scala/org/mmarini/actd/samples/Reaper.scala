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

import scala.concurrent.Future
import scala.concurrent.Promise

import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Terminated

object Reaper {
  def future(actors: Set[ActorRef])(system: ActorSystem): Future[Any] = {

    val promise = Promise[Any]
    reaper(actors, () => { promise.success(None) })(system)
    promise.future
  }

  def reaper(actors: Set[ActorRef], done: () => Unit)(system: ActorSystem): ActorRef =
    system.actorOf(Props(classOf[Reaper], actors, done))
}

class Reaper(actors: Set[ActorRef], done: () => Unit) extends Actor with ActorLogging {
  for { actor <- actors } { context watch actor }

  def receive: Receive = waiting(actors)

  private def waiting(actors: Set[ActorRef]): Receive = {
    case Terminated(s) =>
      val newActors = actors - s
      if (newActors.isEmpty) {
        log.debug("Reaper completed")
        context stop self
        done()
      } else {
        context become waiting(newActors)
      }
  }
}
