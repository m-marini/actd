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

import scala.concurrent.duration.Duration.Zero

import org.mmarini.actd.EnvironmentActor

import com.typesafe.scalalogging.LazyLogging

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object WallTraceApp extends App
    with WallEnvironment
    with ReturnsDump
    //    with AgentSave
    with LazyLogging {

  //  val DelayTime = 200 millis
  val DelayTime = Zero

  private val wArgs = WallArguments(args)
  private val builder = new WallBuilder(wArgs)

  override def windowSize: Int = wArgs.binsSize

  val environment = {
    val agent = builder.initAgent
    val status = builder.initStatus

    val parms = agent.parms
    logger.info(f"Beta=${parms.beta}%g")
    logger.info(f"Gamma=${parms.gamma}%g")
    logger.info(f"Epsilon=${parms.epsilon}%g")
    logger.info(f"Lambda=${parms.lambda}%g")
    logger.info(f"Eta=${parms.eta}%g")
    logger.info(f"L1=${parms.l1}%g")
    logger.info(f"L2=${parms.l2}%g")
    logger.info(f"hiddens=${wArgs.hiddens.mkString(",")}%s")
    logger.info(s"agent=${agent.getClass.getName}")
    logger.info(s"model=${wArgs.modelName}")
    logger.info(f"binsSize=${wArgs.binsSize}%d")
    logger.info(f"stepCount=${wArgs.stepCount}%d")

    system.actorOf(EnvironmentActor.props(status, agent))
  }

  val controllerActor = system.actorOf(TakeActor.props(environment, wArgs.stepCount, DelayTime))

//  val processorActorsSet = Set(returnsActors, saveActors)
  val processorActorsSet = Set(returnsActors)

  startSim

  waitForCompletion
}
