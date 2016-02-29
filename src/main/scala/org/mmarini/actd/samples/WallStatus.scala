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
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status
import org.mmarini.actd.TDNeuralNet
import org.mmarini.actd.TDParms
import org.mmarini.actd.samples.WallStatus.Direction

import com.typesafe.scalalogging.LazyLogging

import WallStatus.PadAction
import breeze.linalg.DenseVector
import breeze.stats.distributions.RandBasis

/** */
case class WallStatus(ball: (Int, Int), direction: Direction.Value, pad: Int) extends Status {

  import PadAction._
  import Direction._
  import WallStatus._

  require(ball._2 >= 0)
  require(ball._2 <= Width)
  require(pad >= 0)
  require(pad <= LastPad)
  require(ball._1 <= Height)
  require(ball._1 >= 0)
  require(ball._1 >= 1 || ball._2 == 0 && direction == SE && pad == 1, s"$ball $direction $pad")

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
      val nextOpt = StatusMap.get((this, PadAction(action)))
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

  type TransitionSource = (WallStatus, PadAction.Value)
  type TransitionTarget = (WallStatus, Double)
  type TransitionMap = Map[TransitionSource, TransitionTarget]

  val Height = 10
  val Width = 13
  val PadSize = 3
  val SecondLastRow = Height - 1
  val PositiveReward = 5.0
  val NegativeReward = -1.0
  val LastCol = Width - 1
  val SecondLastCol = Width - 2
  val LastPad = Width - PadSize
  val SecondLastPad = LastPad - 1

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
    val s = b._2 match {
      case 0 => NE
      case LastCol => NO
      case _ => random.choose(Seq(Direction.NE, Direction.NO)).get
    }
    val pad = b._2 match {
      case 0 => 0
      case c if (c >= LastPad) => LastPad
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

  private def validateTx(s: Seq[(TransitionSource, TransitionTarget)]) = {
    require(s.size == s.toMap.size, s)
    s.toMap
  }

  private def createTx0 =
    validateTx(for {
      pad <- (0 to LastPad)
      dir <- Seq(NO, NE, SO)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((Height, 0), dir, pad)
      val s1 = WallStatus((SecondLastRow, 1), SE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx1 =
    validateTx(for {
      pad <- (0 to LastPad)
      dir <- Seq(NO, NE, SE)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((Height, LastCol), dir, pad)
      val s1 = WallStatus((SecondLastRow, SecondLastCol), SO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx2 =
    validateTx(for {
      c <- 1 to SecondLastCol
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((Height, c), NO, pad)
      val s1 = WallStatus((SecondLastRow, c - 1), SO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx3 =
    validateTx(for {
      c <- 1 to SecondLastCol
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((Height, c), NE, pad)
      val s1 = WallStatus((SecondLastRow, c + 1), SE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx4 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((r, 0), NO, pad)
      val s1 = WallStatus((r + 1, 1), NE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx5 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((r, 0), SO, pad)
      val s1 = WallStatus((r - 1, 1), SE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx6 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((r, LastCol), SE, pad)
      val s1 = WallStatus((r - 1, SecondLastCol), SO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx7 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallStatus((r, LastCol), NE, pad)
      val s1 = WallStatus((r + 1, SecondLastCol), NO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx8 =
    validateTx(for {
      pad <- 1 to SecondLastPad
      c <- pad to pad + 2
    } yield {
      val s0 = WallStatus((1, c), SO, pad)
      val s1 = WallStatus((2, c - 1), NO, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx9 =
    validateTx(for {
      pad <- 1 to SecondLastPad
      c <- pad to pad + 2
    } yield {
      val s0 = WallStatus((1, c), SE, pad)
      val s1 = WallStatus((2, c + 1), NE, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx10 =
    validateTx(for {
      pad <- 0 to LastPad - 2
      c <- pad + 1 to pad + 3
    } yield {
      val s0 = WallStatus((1, c), SO, pad)
      val s1 = WallStatus((2, c - 1), NO, pad + 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx11 =
    validateTx(for {
      pad <- 2 to LastPad
      c <- pad - 1 to pad + 1
    } yield {
      val s0 = WallStatus((1, c), SE, pad)
      val s1 = WallStatus((2, c + 1), NE, pad - 1)
      ((s0, Left), (s1, PositiveReward))
    })

  private def createTx12 =
    validateTx(for {
      dir <- Seq(SO, SE)
      pad <- 0 to 1
    } yield {
      val s0 = WallStatus((1, 0), dir, pad)
      val s1 = WallStatus((2, 1), NE, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx13 =
    validateTx(for {
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallStatus((1, 0), dir, 0)
      val s1 = WallStatus((2, 1), NE, 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx14 =
    validateTx(for {
      pad <- 1 to 2
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallStatus((1, 0), dir, pad)
      val s1 = WallStatus((2, 1), NE, pad - 1)
      ((s0, Left), (s1, PositiveReward))
    })

  /** Create the map of transitions */
  private def createMap: TransitionMap = {
    val lm =
      createTx0 +:
        createTx1 +:
        createTx2 +:
        createTx3 +:
        createTx4 +:
        createTx5 +:
        createTx6 +:
        createTx7 +:
        createTx8 +:
        createTx9 +:
        createTx10 +:
        createTx11 +:
        createTx12 +:
        createTx13 +:
        createTx14 +:
        Seq()
    //
    val lmi = lm.zipWithIndex
    for {
      (li, i) <- lmi
      (lj, j) <- lmi
      if (j > i)
    } {
      val inter = li.keySet & lj.keySet
      require(inter.isEmpty, s"createTx$i & createTx$j = $inter")
    }

    val map = lm.reduce(_ ++ _)
    require(map.size == lm.map(_.size).sum, s"${map.size} != ${lm.map(_.size).sum}")
    map
  }
}
