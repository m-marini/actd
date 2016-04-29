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

import org.mmarini.actd.EnvironmentActor
import org.mmarini.actd.TDAgent

import com.typesafe.scalalogging.LazyLogging

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object FirstEpisodeWallTraceApp extends App with WallEnvironment with AgentSave with FeedbackDump with LazyLogging {

  val environment = {
    val (initStatus, parms, critic, actor) = WallStatus.initEnvParms
    system.actorOf(
      EnvironmentActor.props(initStatus, new TDAgent(parms, critic, actor)))
  }

  val controllerActor = system.actorOf(TakeUntilActor.props(environment, {
    (f, d, a) => f.s1.finalStatus
  }))

  val processorActorsSet = Set(saveActors, feedbackActors)

  startSim

  waitForCompletion

}
