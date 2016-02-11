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

import com.typesafe.scalalogging.LazyLogging
import akka.actor.ActorSystem
import org.mmarini.actd.EnvironmentActor
import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.dispatch.sysmsg.Terminate
import akka.actor.Terminated
import org.mmarini.actd.Feedback
import scala.concurrent.duration.Deadline
import scala.concurrent.duration.Duration
import scala.concurrent.duration._

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object WallTraceApp extends App with LazyLogging {

  val file = "data/wall.csv"
  val StepCount = 1000

  val system = ActorSystem("WallTraceApp")

  val myActor = system.actorOf(Props(classOf[WallTraceActor]))

  class WallTraceActor extends Actor with ActorLogging {
    val (initStatus, parms, critic, actor) = WallStatus.initEnvParms

    val environment = context.actorOf(EnvironmentActor.props(initStatus, parms, critic, actor))

    var counter = StepCount
    var list: Seq[(Feedback, Double)] = Seq()

    context.watch(environment)

    environment ! EnvironmentActor.Interact

    val tlog = new TimerLogger(log)

    def receive: Receive = {
      case EnvironmentActor.Step(feedback, delta) =>
        if (counter > 0) {
          tlog.info(s"$counter counter")
          counter = counter - 1
          list = list :+ (feedback, delta)
          environment ! EnvironmentActor.Interact
        } else {
          context.stop(environment)
        }
      case Terminated(_) =>
        log.info(s"${list.length} size")
        list.
          iterator.
          toSamples.
          write(file)
        context.system.shutdown
    }
  }

  //  val a = WallStatus.
  //    environment.
  //    iterator.
  //    take(StepCount).
  //    traceByInterval("Sample").
  //    toSamples.
  //    write(file)
}
