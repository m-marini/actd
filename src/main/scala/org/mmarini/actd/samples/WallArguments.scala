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

import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.TDParms

import com.typesafe.scalalogging.LazyLogging

import breeze.stats.distributions.RandBasis
import breeze.linalg.DenseVector

/**
 * Set of object build by command line argumenst with default values
 * --beta=3
 * --gamma=0.962
 * --epsilon=5e-3
 * --lambda=0.3
 * --eta=0.1
 * --l1=0
 * --l2=1e-6
 * --seed=1234
 * --hiddens=<comma separated # of neurons>
 * --stepCount=300000
 * --agent=<ACAgent, QAgent>
 * --model=<compact,flat>
 * --bins=1
 */
class WallArguments(args: Array[String]) extends LazyLogging {

  lazy val kvArgs: Map[String, String] = {
    val regex = "--(.*)=(.*)".r
    (for {
      arg <- args
    } yield {
      arg match {
        case regex(key, value) => Some(key -> value)
        case _ => None
      }
    }).filterNot(_.isEmpty).map(_.get).toMap
  }

  private val Beta = "3"
  private val Gamma = "0.962"
  private val EpsilonGreedy = "5e-3"
  private val Lambda = "0.3"
  private val Eta = "0.1"
  private val AgentSeed = "1234"
  private val L1 = "0e-6"
  private val L2 = "1e-6"
  private val Hiddens = ""

  lazy val tdParms: TDParms = {
    val p = kvArgs
    TDParms(
      beta = p.getOrElse("beta", Beta).toDouble,
      gamma = p.getOrElse("gamma", Gamma).toDouble,
      epsilon = p.getOrElse("epsilon", EpsilonGreedy).toDouble,
      lambda = p.getOrElse("lambda", Lambda).toDouble,
      eta = p.getOrElse("eta", Eta).toDouble,
      l1 = p.getOrElse("l1", L1).toDouble,
      l2 = p.getOrElse("l2", L2).toDouble,
      random = new RandBasis(new MersenneTwister(p.getOrElse("seed", AgentSeed).toLong)))
  }

  lazy val hiddens: Seq[Int] = {
    val s = kvArgs.getOrElse("hiddens", Hiddens)
    if (s.isEmpty) Seq() else s.split(",").map(_.toInt)
  }

  lazy val agent: String = kvArgs.getOrElse("agent", "ACAgent")

  lazy val stepCount: Int = kvArgs.getOrElse("stepCount", "300000").toInt

  lazy val binsSize: Int = kvArgs.getOrElse("binsSize", "1").toInt

  lazy val modelName: String = kvArgs.getOrElse("model", "flat")

  lazy val model: WallStatus => DenseVector[Double] = modelName match {
    case "compact" => CompactWallStatus.toDenseVector
    case "flat" => FlatWallStatus.toDenseVector
  }
}

object WallArguments {
  def apply(args: Array[String] = Array()): WallArguments = new WallArguments(args)
}
