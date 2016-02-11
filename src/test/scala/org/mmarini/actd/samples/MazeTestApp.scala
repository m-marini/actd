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

import java.io.File

import scala.collection.immutable.Stream.consWrapper

import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.Environment
import org.mmarini.actd.Feedback
import org.mmarini.actd.TDAgent
import org.mmarini.actd.TDParms

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg.DenseMatrix
import breeze.linalg.csvwrite
import breeze.stats.distributions.RandBasis

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object MazeTestApp extends App with LazyLogging {

  val file = "data/maze.csv"

  val Alpha = 10e-3
  val Beta = 0.1
  val Gamma = 1
  val Epsilon = 0.1
  val Lambda = 0.9
  val Eta = 1e-1
  val Sigma = 1.0
  val Seed = 123L
  val EpisodeCount = 100

  val OutputCount = 4

  val FieldSize = 5
  val initStatus = GraphStatus.flatFieldMaze(FieldSize, FieldSize)

  val inputCount = initStatus.toDenseVector.length

  val initAgent = TDAgent(
    TDParms(
      alpha = Alpha,
      beta = Beta,
      gamma = Gamma,
      epsilon = 0.0,
      lambda = Lambda,
      eta = Eta,
      random = new RandBasis(new MersenneTwister(Seed))),
    Sigma,
    inputCount,
    OutputCount)

  val initEnv = Environment(initStatus, initAgent)

  private def extractEpisodeRewards: Iterator[Stream[(Environment, Environment, Feedback, Double)]] = {
    def createStream(s: Iterator[(Environment, Environment, Feedback, Double)]): Iterator[Stream[(Environment, Environment, Feedback, Double)]] = {
      //      val endIdx = s.indexWhere {
      //        case (e0, _, _) => e0.status.finalStatus
      //      }
      //      val (episode, tail) = s.splitAt(endIdx + 1)
      //      episode #:: createStream(tail)
      ???
    }

    val y = createStream(initEnv.iterator)
    y.zipWithIndex.map(x => {
      logger.info(s"episode ${x._2}")
      x._1
    })
  }

  private def extractReturns: Iterator[Double] =
    extractEpisodeRewards.map(_.map(_._3.reward).sum)

  /** Generates the report */
  private def generateReport: DenseMatrix[Double] =
    new DenseMatrix(EpisodeCount, 1, extractReturns.take(EpisodeCount).toArray)

  csvwrite(new File(file), generateReport)
}
