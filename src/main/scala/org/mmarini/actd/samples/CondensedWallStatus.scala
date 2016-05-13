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

import scala.math.sqrt

import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status
import org.mmarini.actd.TDNeuralNet
import org.mmarini.actd.TDParms

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg.DenseVector
import breeze.linalg.InjectNumericOps
import breeze.stats.distributions.RandBasis

/** The status of wall game */
case class CondensedWallStatus(status: WallStatus) extends Status {

  import CondensedWallStatus._
  /**
   * Transforms the status into a Vector
   * The resulting vector consists of (rows x columns) signals for the ball position,
   * 4 signals for the ball direction,
   * (columns - padsize + 1) signals for pad location
   * and a signal for final status
   */
  val toDenseVector: DenseVector[Double] = {
    if (status.finalStatus) {
      FinalVector
    } else {
      normalizeStatus(DenseVector[Double](
        status.ball._1,
        status.ball._2,
        status.direction.id,
        status.pad,
        -1))
    }
  }

  /** Produce the feedback of an applied action */
  def apply(action: Action): Feedback = status(action) match {
    case (_, action, reward, s1) => Feedback(this, action, reward, CondensedWallStatus(s1))
  }
}

/** Factory of [[WallStatusDeep]] */
object CondensedWallStatus extends LazyLogging {

  import WallStatus._

  object StatusIndex extends Enumeration {
    val BallColumIdx, BallRowIdx, BallDrectionIdx, PadIdx, FinalIdx = Value
  }
  val InputSpaceDimension = StatusIndex.maxId

  private val MinValues = DenseVector[Double](0, 0, 0, 0, -1)
  private val MaxValues = DenseVector[Double](Height + 1, Width, Direction.maxId, LastPad, 1)
  private val Scale = sqrt(3) * 2 * DenseVector.ones[Double](InputSpaceDimension) :/ (MaxValues - MinValues)
  private val Mean = (MaxValues - MinValues) / 2.0

  val FinalVector = normalizeStatus(DenseVector[Double](
    0,
    0,
    0,
    0,
    1))

  FinalVector.update(0, 1.0)

  val L1 = 0e-6
  val L2 = 0e-6
  val Beta = 3
  val Gamma = 0.962
  //  val EpsilonGreedy = 0.9
  val EpsilonGreedy = 5e-3
  val Lambda = 0.3
  val Eta = 0.3
  val Seed = 1234L
  val EnvSeed = 4321L

  val OutputCount = 3

  //  val HiddenLayer1Count = 100
  //  val HiddenLayerCount = Seq(HiddenLayer1Count)
  val HiddenLayer1Count = 10
  val HiddenLayer2Count = 100
  val HiddenLayerCount = Seq(HiddenLayer1Count, HiddenLayer1Count)

  /** Creates a initial environment parameters */
  def initEnvParms: (CondensedWallStatus, TDParms, TDNeuralNet, TDNeuralNet) = {

    val initStatus = CondensedWallStatus(WallStatus.initial)

    val inputCount = initStatus.toDenseVector.length

    val parms = TDParms(
      beta = Beta,
      gamma = Gamma,
      epsilon = EpsilonGreedy,
      lambda = Lambda,
      eta = Eta,
      l1 = L1,
      l2 = L2,
      random = new RandBasis(new MersenneTwister(Seed)))

    val critic = TDNeuralNet(parms)(inputCount +: HiddenLayerCount :+ 1)
    val actor = TDNeuralNet(parms)(inputCount +: HiddenLayerCount :+ OutputCount)

    (initStatus, parms, critic, actor)
  }

  def normalizeStatus(x: DenseVector[Double]): DenseVector[Double] = Scale :* (x - Mean)
}
