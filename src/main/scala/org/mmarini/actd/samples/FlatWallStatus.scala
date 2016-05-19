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

import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status

import com.typesafe.scalalogging.LazyLogging

import WallStatus.Height
import WallStatus.LastPad
import WallStatus.Width
import breeze.linalg.DenseVector
import org.mmarini.actd.TDNeuralNet
import org.mmarini.actd.TDParms
import org.mmarini.actd.samples.WallStatus.PadAction
import breeze.stats.distributions.RandBasis
import org.apache.commons.math3.random.MersenneTwister

/** The status of wall game */
case class FlatWallStatus(status: WallStatus) extends Status {

  import FlatWallStatus._

  /** Transforms the status to a Vector */
  val toDenseVector: DenseVector[Double] = {
    if (finalStatus) {
      FinalVector
    } else {

      val v = DenseVector.zeros[Double](BallDim * SpeedDim * PadDim + 1)

      val ballIdx = (status.ball._1 - 1) * Width + status.ball._2
      val speedIdx = status.direction.id

      val idx = ballIdx + speedIdx * BallDim + status.pad * (BallDim * SpeedDim)

      v.update(idx, 1.0)

      v
    }
  }

  /** Produce the feedback of an applied action */
  def apply(action: Action): Feedback = status(action) match {
    case (_, action, reward, s1) => Feedback(this, action, reward, FlatWallStatus(s1))
  }

  /** Returns true if is a final status */
  override def finalStatus: Boolean = status.finalStatus
}

/** A factory of [[WallStatus]] */
object FlatWallStatus extends LazyLogging {
  private val BallDim = Width * Height
  private val SpeedDim = 4
  private val PadDim = LastPad + 1
  private val FinalVector = DenseVector.zeros[Double](BallDim * SpeedDim * PadDim + 1)

  val OutputCount = PadAction.maxId

  /** Creates a initial environment parameters */
  def initEnvParms(args: WallArguments): (FlatWallStatus, TDParms, TDNeuralNet, TDNeuralNet) = {

    val initStatus = FlatWallStatus(WallStatus.initial)

    val inputCount = initStatus.toDenseVector.length

    val critic = TDNeuralNet(args.tdParms)(inputCount +: args.hiddens :+ 1)
    val actor = TDNeuralNet(args.tdParms)(inputCount +: args.hiddens :+ OutputCount)

    (initStatus, args.tdParms, critic, actor)
  }

}
