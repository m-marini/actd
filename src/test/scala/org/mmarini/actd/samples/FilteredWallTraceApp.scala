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

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import org.mmarini.actd.EnvironmentActor
import org.mmarini.actd.Feedback
import org.mmarini.actd.Feedback
import org.mmarini.actd.Feedback
import com.typesafe.scalalogging.LazyLogging
import akka.actor.ActorLogging
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.pattern.ask
import akka.util.Timeout
import breeze.linalg.DenseVector
import org.mmarini.actd.EnvironmentActor.Interact
import org.mmarini.actd.ProxyActor
import org.mmarini.actd.EnvironmentActor.Step
import scala.util.Try

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object FilteredWallTraceApp extends App with LazyLogging {

  val File = "data/debug-wall.csv"
  val EpisodeCount = 100
  val TimeLimit = 10 hours

  val system = ActorSystem()

  val (initStatus, parms, critic, actor) = WallStatus.initEnvParms

  val environment = system.actorOf(
    EnvironmentActor.props(initStatus, parms, critic, actor))

  val filter = system.actorOf(ProxyActor.filterProps(environment, Interact) {
    case Step(Feedback(WallStatus((9, 6), (1, -1), 3), _, _, _), _) => true
    case _ => false
  })

  val takeActor = system.actorOf(TakeActor.props(filter, EpisodeCount))

  implicit val timeout = Timeout(TimeLimit)

  val f = (takeActor ask None).mapTo[Seq[(Feedback, Double)]]

  try {
    Await.result(f, TimeLimit).
      iterator.
      toSamples.
      write(File)
  } catch {
    case x: Throwable => logger.error("Error", x)
  }

  system stop environment

  system.terminate
}

//
//  /*
//     * Filter on the following status:
//     *
//     *   8 |     o .  |
//     *   9 |      O   |
//     *  10 |   ---    |
//     *      0123456789
//     */
//  private def filter3(x: DenseVector[Double]) =
//    x(RowIdx) == 9 &&
//      x(ColIdx) == 6 &&
//      x(RowSpeedIdx) == 1 &&
//      x(ColSpeedIdx) == -1 &&
//      x(PadIdx) == 3
//
//  /*
//     * Filter on the following status:
//     *
//     *   8 |       .  |
//     *   9 |      O   |
//     *  10 |  ---o    |
//     *      0123456789
//     */
//  private def filter1(x: DenseVector[Double]) =
//    x(RowIdx) == 9 &&
//      x(ColIdx) == 6 &&
//      x(RowSpeedIdx) == 1 &&
//      x(ColSpeedIdx) == -1 &&
//      x(PadIdx) == 2
//
//  /*
//     * Filter on the following status:
//     *
//     *   8 |       O  |
//     *   9 |      o   |
//     *  10 |  ---o    |
//     *      0123456789
//     */
//  private def filter(x: DenseVector[Double]) =
//    x(RowIdx) == 8 &&
//      x(ColIdx) == 7 &&
//      x(RowSpeedIdx) == 1 &&
//      x(ColSpeedIdx) == -1 &&
//      x(PadIdx) == 2
//
//  /*
//     * Filter on the following status
//     *
//     *   8  O
//     *   9   o
//     *  10    o---
//     *      234567
//     */
//  private def filter2(x: DenseVector[Double]) =
//    x(RowIdx) == 8 && x(ColIdx) == 2 && x(RowSpeedIdx) == 1 && x(ColSpeedIdx) == 1 && x(PadIdx) == 5
//
//  /** Generates the report */
//  //  WallStatus.environment.iterator.
//  //    toSamplesWithAC.
//  //    trace("Sample", SampleTraceCount).
//  //    filter(filter).
//  //    trace("Filtered").
//  //    take(EpisodeCount).
//  //    write(file)
//}
