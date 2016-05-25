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
import scala.math.pow
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
 * --binsSize=1
 * --halving=300000
 */
class WallArguments(kvArgs: Map[String, String]) extends LazyLogging {

  lazy val tdParms: TDParms = {
    val p = kvArgs
    val halving: Int = kvArgs("halving").toInt
    val decayEta: Double = pow(0.5, 1.0 / halving)
    TDParms(
      beta = kvArgs("beta").toDouble,
      gamma = kvArgs("gamma").toDouble,
      epsilon = kvArgs("epsilon").toDouble,
      lambda = kvArgs("lambda").toDouble,
      eta = kvArgs("eta").toDouble,
      l1 = kvArgs("l1").toDouble,
      l2 = kvArgs("l2").toDouble,
      decayEta = decayEta,
      random = new RandBasis(new MersenneTwister(p("seed").toLong)))
  }

  lazy val hiddens: Seq[Int] = {
    val s = kvArgs("hiddens")
    if (s.isEmpty) Seq() else s.split(",").map(_.toInt)
  }

  lazy val agent: String = kvArgs("agent")

  lazy val stepCount: Int = kvArgs("stepCount").toInt

  lazy val binsSize: Int = kvArgs("binsSize").toInt

  lazy val modelName: String = kvArgs("model")

  lazy val model: WallStatus => DenseVector[Double] = modelName match {
    case "compact" => CompactWallStatus.toDenseVector
    case "flat" => FlatWallStatus.toDenseVector
  }

  /** */
  def dump: WallArguments = {
    for { (key, value) <- kvArgs.toSeq.sortBy(_._1) }
      logger.info(s"${key}=${value}");
    this
  }
}

object WallArguments {
  private val DefaultArgs = Map(
    "halving" -> "300000",
    "beta" -> "3",
    "gamma" -> "0.962",
    "epsilon" -> "5e-3",
    "lambda" -> "0.3",
    "eta" -> "0.1",
    "seed" -> "1234",
    "l1" -> "0",
    "l2" -> "1e-6",
    "hiddens" -> "",
    "agent" -> "ACAgent",
    "stepCount" -> "300000",
    "binsSize" -> "1",
    "model" -> "flat")

  def apply(args: Array[String] = Array(), defaultArgs: Map[String, String] = DefaultArgs): WallArguments = {
    val regex = "--(.*)=(.*)".r
    val kvArgs: Map[String, String] =
      (for {
        arg <- args
      } yield {
        arg match {
          case regex(key, value) if (defaultArgs.contains(key)) => Some(key -> value)
          case _ => None
        }
      }).filterNot(_.isEmpty).map(_.get).toMap
    new WallArguments(defaultArgs ++ kvArgs)
  }
}
