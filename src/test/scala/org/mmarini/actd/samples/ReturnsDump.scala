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

import scala.concurrent.duration.DurationInt

import org.mmarini.actd.VectorIteratorFactory

import com.typesafe.scalalogging.LazyLogging

import akka.actor.Actor.Receive
import akka.actor.ActorRef
import akka.actor.ActorSystem
import breeze.linalg.DenseVector

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
trait ReturnsDump extends LazyLogging {

  def system: ActorSystem

  val returnsFilename = "data/returns.csv"
  val dumpInterval = 10 minutes
  val windowSize = 1

  lazy val returnsActors: Seq[ActorRef] = {
    val consumeActor = system.actorOf(ConsumerActor.props(consume))
    val toSeqActor = system.actorOf(ToSeqActor.props(dumpInterval, consumeActor))
    val list = if (windowSize == 1) {
      Seq(toSeqActor)
    } else {
      val meanActor = system.actorOf(MapperActor.props(toSeqActor, mean))
      val windowActor = system.actorOf(WindowActor.props(meanActor, windowSize))
      Seq(windowActor, meanActor, toSeqActor)
    }
    val returnsActor = system.actorOf(ReturnsActor.props(list.head))
    returnsActor +: list :+ consumeActor
  }

  private def mean(x: Any): Any = x match {
    case buffer: Seq[DenseVector[Double]] => buffer.reduce((a, b) => a + b) / buffer.length.toDouble
  }

  private def consume: Receive = {
    case msg: Seq[Any] =>
      logger.info(s"Dump returns into $returnsFilename")
      msg.asInstanceOf[Seq[DenseVector[Double]]].
        iterator.
        write(returnsFilename)
  }

}
