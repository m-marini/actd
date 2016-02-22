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
    val padDim = Width - PadSize + 1

    val v = DenseVector.zeros[Double](ballDim * speedDim * padDim)

    val ballIdx = ball._1 * Width + ball._2
    val speedIdx = direction.id

    val idx = ballIdx + speedIdx * ballDim + pad * (ballDim * speedDim)
    v.update(idx, 1.0)

    v
  }

  /** Computes the position and speed after a bounce in a vertical wall */
  private def hBounce(x: ((Int, Int), (Int, Int))) = {
    val ((r, c), (sr, sc)) = x
    (c + sc) match {
      case -1 => ((r, 1), (sr, 1))
      case Width => ((r, Width - 2), (sr, -1))
      case c1 => ((r, c1), (sr, sc))
    }
  }

  /** Computes the position and speed after a bounce in a horizontal wall */
  private def vBounce(x: ((Int, Int), (Int, Int))) = {
    val ((r, c), (sr, sc)) = x
    (r + sr) match {
      case -1 => ((1, c), (1, sc))
      case r => ((r, c), (sr, sc))
    }
  }

  /** */
  private def movePad(action: Action) = PadAction.apply(action) match {
    case Left if pad > 0 => pad - 1
    case Right if pad < Width - PadSize => pad + 1
    case _ => pad
  }

  /** */
  def apply(action: Action): Feedback = {

    def nextStatus: (WallStatus, Double) = {
      val pad1 = movePad(action)
      if (finalStatus) {
        // Restarts because ball is out of field
        (WallStatus.initial, 0.0)
      } else {
        this match {

          case WallStatus((2, 1), SO, _) => (WallStatus((1, 0), NE, pad1), PositiveReward)

          case WallStatus((2, 0), SE, _) => (WallStatus((1, 1), NE, pad1), PositiveReward)

          case WallStatus((2, c), SE, _) if (c >= 1 && c <= Width - PadSize - 1 && pad1 >= c - 1 && pad1 <= c + 1) =>
            (WallStatus((1, c + 1), NE, pad1), PositiveReward)

          case WallStatus((2, LastCol2), SE, _) if (pad1 >= Width - PadSize - 1) =>
            (WallStatus((1, 11), NE, pad1), PositiveReward)

          case WallStatus((2, LastCol1), SE, _) if (pad1 == Width - PadSize) =>
            (WallStatus((1, 12), NO, pad1), PositiveReward)

          case WallStatus((2, 2), SO, _) if (pad1 <= 1) =>
            (WallStatus((1, 1), NO, pad1), PositiveReward)

          case WallStatus((2, c), SO, _) if (c >= 3 && c <= Width - PadSize + 1 && pad1 >= c - 3 && pad1 <= c - 1) =>
            (WallStatus((1, c - 1), NO, pad1), PositiveReward)

          case WallStatus((2, LastCol), SO, _) if (pad1 >= Width - PadSize - 1) =>
            (WallStatus((1, 11), NO, pad1), PositiveReward)

          case WallStatus((2, Col4), SO, _) if (pad1 == 0) =>
            (WallStatus((1, 11), NO, pad1), PositiveReward)

          case WallStatus((r, 1), SO, _) if (r >= 3) =>
            (WallStatus((r - 1, 0), SE, pad1), 0.0)

          case WallStatus((r, LastCol1), SE, _) if (r >= 3) =>
            (WallStatus((r - 1, Width - 1), SO, pad1), 0.0)

          case WallStatus((LastRow1, c), NE, _) if (c <= 10) =>
            (WallStatus((Height, c + 1), SE, pad1), 0.0)

          case WallStatus((LastRow1, c), NO, _) if (c >= 2) =>
            (WallStatus((Height, c - 1), SO, pad1), 0.0)

          case WallStatus((LastRow1, 1), NO, _) =>
            (WallStatus((Height, 0), SE, pad1), 0.0)

          case WallStatus((LastRow1, LastCol1), NE, _) =>
            (WallStatus((Height, Width - 1), SO, pad1), 0.0)

          case _ =>
            (this, 0.0)
        }
      }
    }

    nextStatus match {
      case (s1, reward) => Feedback(this, action, reward, s1)
    }

    //    if (finalStatus) {
    //      // Restarts because ball is out of field
    //      Feedback(this, action, 0.0, )
    //    } else if (ball._1 < Height - 1 || speed._1 < 0) {
    //      // Ball in the field
    //      val (nb, ns) = hBounce(vBounce(ball, speed))
    //      Feedback(this, action, 0.0, WallStatus(nb, ns, movePad(action)))
    //    } else if (ball._2 >= pad && ball._2 <= pad + 2 ||
    //      ball._2 == pad - 1 && speed._2 == 1 ||
    //      ball._2 == pad + 3 && speed._2 == -1) {
    //      val (nb, ns) = hBounce(vBounce((ball, (-1, speed._2))))
    //      Feedback(this, action, PositiveReward, WallStatus(nb, ns, movePad(action)))
    //    } else {
    //      val (nb, ns) = hBounce(vBounce(ball, speed))
    //      Feedback(this, action, NegativeReward, WallStatus(nb, ns, movePad(action)))
    //    }
  }

  /** */
  override def finalStatus: Boolean = ball._1 == 0

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
  val LastCol1 = Width - 2
  val LastCol2 = Width - 3

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

  val random = new RandBasis(new MersenneTwister(Seed))

  /** MazeAction */
  object PadAction extends Enumeration {
    val Rest, Left, Right = Value
  }

  object Direction extends Enumeration {
    val NO, NE, SE, SO = Value
  }

  import PadAction._

  /** Creates a initial game status */
  def initial: WallStatus = {
    val b = (Height - 1, random.randInt(Width).get)
    val s = random.choose(Seq(Direction.NE, Direction.NO)).get
    val pad = b._2 match {
      case 0 => 0
      case c if (c - 1 >= Width - PadSize) => Width - PadSize
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
}
