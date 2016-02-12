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

import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.actor.Terminated
import akka.actor.PoisonPill
import org.mmarini.actd.EnvironmentActor.Interact

object ProxyActor {
  def filterProps(
    source: ActorRef,
    target: ActorRef)(filter: Any => Boolean = _ => true): Props =
    Props(classOf[FilterActor], source, target, filter)

  def mapFProps(
    source: ActorRef,
    target: ActorRef)(map: Any => Any = x => x): Props =
    Props(classOf[MapActor], source, target, map)
}

/**
 *
 */
class FilterActor(
    source: ActorRef,
    target: ActorRef,
    filter: Any => Boolean) extends Actor {

  context watch source
  context watch target
  def receive: Receive = {
    case Terminated(`source`) =>
      context stop target
      context stop self
    case Terminated(`target`) =>
      context stop source
      context stop self
    case msg if (sender == source) =>
      if (filter(msg)) target ! msg else source ! Interact
    case msg if (sender == target) => source ! msg
  }
}

/**
 *
 */
class MapActor(
    source: ActorRef,
    target: ActorRef,
    map: Any => Any) extends Actor {
  def receive: Receive = {
    case Terminated(`source`) =>
      context stop target
      context stop self
    case Terminated(`target`) =>
      context stop source
      context stop self
    case msg if (sender == source) => target ! map(msg)
    case msg if (sender == target) => source ! msg
  }
}
