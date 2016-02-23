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
import org.mmarini.actd.Action
import org.mmarini.actd.DummyAgent
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status
import org.mmarini.actd.TDAgent
import org.mmarini.actd.TDNeuralNet
import org.mmarini.actd.TDParms
import com.typesafe.scalalogging.LazyLogging
import WallStatus.Height
import WallStatus.NegativeReward
import WallStatus.PadAction
import WallStatus.PadSize
import WallStatus.PositiveReward
import WallStatus.Width
import breeze.linalg.DenseVector
import breeze.stats.distributions.RandBasis
import breeze.optimize.MaxIterations
import org.mmarini.actd.Feedback
import org.mmarini.actd.samples.WallStatus.Direction

/** */
case class WallStatus(ball: (Int, Int), direction: Direction.Value, pad: Int) extends Status {

  import PadAction._
  import Direction._
  import WallStatus._

  /** */
  val toDenseVector: DenseVector[Double] = {
    val ballDim = Width * (Height + 1)
    val speedDim = 4
    val padDim = LastPad + 1

    val v = DenseVector.zeros[Double](ballDim * speedDim * padDim)

    val ballIdx = ball._1 * Width + ball._2
    val speedIdx = direction.id

    val idx = ballIdx + speedIdx * ballDim + pad * (ballDim * speedDim)
    v.update(idx, 1.0)

    v
  }

  /** Returns a [[WallStatus]] with changed pad location */
  def pad(x: Int): WallStatus = WallStatus(ball, direction, x)

  /** */
  private def movePad(action: Action) = PadAction.apply(action) match {
    case Left if pad > 0 => pad - 1
    case Right if pad < LastPad => pad + 1
    case _ => pad
  }

  /** */
  def apply(action: Action): Feedback = {
    val pad1 = movePad(action)
    val (s1, reward) = if (finalStatus) {
      // Restarts because ball is out of field
      (WallStatus.initial, 0.0)
    } else {
      val nextOpt = StatusMap.get((this.pad(pad1)))
      nextOpt.getOrElse((
        direction match {
          case NO => WallStatus((ball._1 + 1, ball._2 - 1), direction, pad1)
          case NE => WallStatus((ball._1 + 1, ball._2 + 1), direction, pad1)
          case SO => WallStatus((ball._1 - 1, ball._2 - 1), direction, pad1)
          case SE => WallStatus((ball._1 - 1, ball._2 + 1), direction, pad1)
        }, 0.0));
    }
    Feedback(this, action, reward, s1)
  }

  /** */
  override def finalStatus: Boolean = this == endStatus

}

object WallStatus extends LazyLogging {

  val Height = 10
  val Width = 13
  val PadSize = 3
  val LastRow1 = Height - 1
  val PositiveReward = 5.0
  val NegativeReward = -1.0
  val Col4 = 4
  val LastCol = Width - 1
  val SecondLastCol = Width - 2
  val LastPad = Width - PadSize
  val SecondLastPad = LastPad - 1
  //  val LastCol2 = Width - 3

  val Alpha = 100e-6
  val Beta = 0.3
  val Gamma = 0.962
  val Epsilon = 0.1
  //  val EpsilonGreedy = 0.9
  val EpsilonGreedy = 0.1
  val Lambda = 0e-3
  val Eta = 100e-3
  val Sigma = 1.0
  val Seed = 1234L
  val MaxTrainingSamples = 100

  val OutputCount = 3
  val HiddenCount = 20

  /** MazeAction */
  object PadAction extends Enumeration {
    val Rest, Left, Right = Value
  }

  object Direction extends Enumeration {
    val NO, NE, SE, SO = Value
  }
  import PadAction._
  import Direction._

  val random = new RandBasis(new MersenneTwister(Seed))

  val endStatus = WallStatus((0, 0), SE, 1)

  val StatusMap = createMap

  /** Creates a initial game status */
  def initial: WallStatus = {
    val b = (Height - 1, random.randInt(Width).get)
    val s = random.choose(Seq(Direction.NE, Direction.NO)).get
    val pad = b._2 match {
      case 0 => 0
      case c if (c - 1 >= LastPad) => LastPad
      case c => c - 1
    }
    WallStatus(b, s, pad)
  }

  def initEnvParms: (WallStatus, TDParms, TDNeuralNet, TDNeuralNet) = {

    val initStatus = WallStatus.initial

    val inputCount = initStatus.toDenseVector.length

    val parms = TDParms(
      alpha = Alpha,
      beta = Beta,
      gamma = Gamma,
      epsilon = EpsilonGreedy,
      lambda = Lambda,
      eta = Eta,
      maxTrainingSamples = MaxTrainingSamples,
      random = new RandBasis(new MersenneTwister(Seed)))

    val critic = TDNeuralNet(inputCount +: Seq() :+ 1, parms, Sigma)
    val actor = TDNeuralNet(inputCount +: Seq() :+ OutputCount, parms, Sigma)

    (initStatus, parms, critic, actor)
  }

  /** Create the map of transitions in the borders */
  private def createBordersMap: Map[WallStatus, (WallStatus, Double)] = {
    val m9 = for {
      r <- 3 to Height
      pad1 <- 0 to LastPad
    } yield (WallStatus((r, 1), SO, pad1) -> (WallStatus((r - 1, 0), SE, pad1), 0.0))

    val m10 = for {
      r <- 3 to Height
      pad1 <- 0 to LastPad
    } yield (WallStatus((r, SecondLastCol), SE, pad1) -> (WallStatus((r - 1, Width - 1), SO, pad1), 0.0))

    val m11 = for {
      c <- 0 to Width - 3
      pad1 <- 0 to LastPad
    } yield (WallStatus((LastRow1, c), NE, pad1) -> (WallStatus((Height, c + 1), SE, pad1), 0.0))

    val m12 = for {
      c <- 2 to Width - 1
      pad1 <- 0 to LastPad
    } yield (WallStatus((LastRow1, c), NO, pad1) -> (WallStatus((Height, c - 1), SO, pad1), 0.0))

    val m13 = for {
      pad1 <- 0 to LastPad
    } yield (WallStatus((Height - 1, 1), NO, pad1) -> (WallStatus((Height, 0), SE, pad1), 0.0))

    val m14 = for {
      pad1 <- 0 to LastPad
    } yield (WallStatus((LastRow1, SecondLastCol), NE, pad1) -> (WallStatus((Height, Width - 1), SO, pad1), 0.0))

    val m15 = for {
      pad1 <- 0 to LastPad
    } yield (WallStatus((LastRow1, 1), NO, pad1) -> (WallStatus((Height, 0), SE, pad1), 0.0))
    (m9 ++ m10 ++ m11 ++ m12 ++ m13 ++ m14 ++ m15).toMap
  }

  /** Create the map of bounce transitions */
  private def createBounceMap: Map[WallStatus, (WallStatus, Double)] = {
    val m1 = for {
      pad1 <- 0 to LastPad
    } yield (WallStatus((2, 1), SO, pad1) -> (WallStatus((1, 0), NE, pad1), PositiveReward))

    val m2 = for {
      pad1 <- 0 to LastPad
    } yield (WallStatus((2, 0), SE, pad1) -> (WallStatus((1, 1), NE, pad1), PositiveReward))

    val m3 = for {
      c <- 1 to SecondLastPad
      pad1 <- c - 1 to c + 1
    } yield (WallStatus((2, c), SE, pad1) -> (WallStatus((1, c + 1), NE, pad1), PositiveReward))

    val m4 = for {
      pad1 <- SecondLastPad to LastPad
    } yield (WallStatus((2, Width - 3), SE, pad1) -> (WallStatus((1, 11), NE, pad1), PositiveReward))

    val m5 = Map(WallStatus((2, SecondLastCol), SE, LastPad) -> (WallStatus((1, 12), NO, LastPad), PositiveReward))

    val m6 = for {
      pad1 <- 0 to 1
    } yield (WallStatus((2, 2), SO, pad1) -> (WallStatus((1, 1), NO, pad1), PositiveReward))

    val m7 = for {
      c <- 3 to LastPad + 1
      pad1 <- c - 3 to c - 1
    } yield (WallStatus((2, c), SO, pad1) -> (WallStatus((1, c - 1), NO, pad1), PositiveReward))

    val m8 = for {
      pad1 <- SecondLastPad to LastPad
    } yield (WallStatus((2, LastCol), SO, pad1) -> (WallStatus((1, LastCol - 1), NO, pad1), PositiveReward))

    val m16 = Map(WallStatus((2, 4), SO, 0) -> (WallStatus((1, 3), NE, 0), PositiveReward))

    val m17 = Map(WallStatus((2, 8), SE, LastPad) -> (WallStatus((1, 9), NO, LastPad), PositiveReward))

    (m1 ++ m2 ++ m3 ++ m4 ++ m5 ++ m6 ++ m7 ++ m8 ++ m16 ++ m17).toMap
  }

  /** Create the map of missing transitions */
  private def createMissingMap: Map[WallStatus, (WallStatus, Double)] = {
    val m1 = for {
      c <- 0 to LastPad - 2
      pad1 <- c + 2 to LastPad
    } yield (WallStatus((1, c), SE, pad1) -> (endStatus, NegativeReward))

    val m2 = for {
      c <- 3 to Width - 1
      pad1 <- 0 to c - 3
    } yield (WallStatus((1, c), SE, pad1) -> (endStatus, NegativeReward))

    val m3 = for {
      c <- 1 to SecondLastPad
      pad1 <- c + 1 to LastPad
    } yield (WallStatus((1, c), SO, pad1) -> (endStatus, NegativeReward))

    val m4 = for {
      c <- 4 to Width - 1
      pad1 <- 0 to c - 4
    } yield (WallStatus((1, c), SO, pad1) -> (endStatus, NegativeReward))

    (m1 ++ m2 ++ m3 ++ m4).toMap
  }

  /** Create the map of transitions */
  private def createMap: Map[WallStatus, (WallStatus, Double)] = createBounceMap ++ createBordersMap ++ createMissingMap
}
