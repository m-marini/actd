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
import org.mmarini.actd.samples.WallDeepStatus.Direction
import org.mmarini.actd.samples.WallDeepStatus.PadAction

import com.typesafe.scalalogging.LazyLogging

import WallDeepStatus.PadAction
import breeze.linalg.DenseVector
import breeze.stats.distributions.RandBasis

import scala.math.sqrt

/** The status of wall game */
case class WallDeepStatus(ball: (Int, Int), direction: Direction.Value, pad: Int) extends Status {

  import PadAction._
  import Direction._
  import WallDeepStatus._

  require(ball._2 >= 0)
  require(ball._2 <= Width)
  require(pad >= 0)
  require(pad <= LastPad)
  require(ball._1 <= Height)
  require(ball._1 >= 0)
  require(ball._1 >= 1 || ball._2 == 0 && direction == SE && pad == 1, s"$ball $direction $pad")

  /**
   * Transforms the status into a Vector
   * The resulting vector consists of (rows x columns) signals for the ball position,
   * 4 signals for the ball direction,
   * (columns - padsize + 1) signals for pad location
   * and a signal for final status
   */
  val toDenseVector: DenseVector[Double] = {
    if (finalStatus) {
      FinalVector
    } else {
      normalizeStatus(DenseVector[Double](
        ball._1,
        ball._2,
        direction.id,
        pad,
        -1))
    }
  }

  /** Returns a [[WallStatusDeep]] with changed pad location */
  def pad(x: Int): WallDeepStatus = WallDeepStatus(ball, direction, x)

  /** Moves the pad by action */
  private def movePad(action: Action) = PadAction.apply(action) match {
    case Left if pad > 0 => pad - 1
    case Right if pad < LastPad => pad + 1
    case _ => pad
  }

  /** Produce the feedback of an applied action */
  def apply(action: Action): Feedback = {
    val pad1 = movePad(action)
    val (s1, reward) = if (finalStatus) {
      // Restarts because ball is out of field
      (WallDeepStatus.initial, 0.0)
    } else {
      val nextOpt = StatusMap.get((this, PadAction(action)))
      nextOpt.getOrElse(
        direction match {
          case NO => (WallDeepStatus((ball._1 + 1, ball._2 - 1), direction, pad1), 0.0)
          case NE => (WallDeepStatus((ball._1 + 1, ball._2 + 1), direction, pad1), 0.0)
          case SO if (ball._1 == 1) => (endStatus, NegativeReward)
          case SE if (ball._1 == 1) => (endStatus, NegativeReward)
          case SO => (WallDeepStatus((ball._1 - 1, ball._2 - 1), direction, pad1), 0.0)
          case SE => (WallDeepStatus((ball._1 - 1, ball._2 + 1), direction, pad1), 0.0)
        })
    }
    Feedback(this, action, reward, s1)
  }

  /** Returns true if is a final status */
  override def finalStatus: Boolean = this == endStatus

}

/** Factory of [[WallStatusDeep]] */
object WallDeepStatus extends LazyLogging {

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

  //  private val BallDim = Width * Height
  private val SpeedDim = 4
  private val PadDim = LastPad + 1

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

  type TransitionSource = (WallDeepStatus, PadAction.Value)
  type TransitionTarget = (WallDeepStatus, Double)
  type TransitionMap = Map[TransitionSource, TransitionTarget]

  val L1 = 0e-6
  val L2 = 1e-6
  val Beta = 3
  val Gamma = 0.962
  //  val EpsilonGreedy = 0.9
  val EpsilonGreedy = 5e-3
  val Lambda = 0.3
  val Eta = 0.1
  val Seed = 1234L
  val EnvSeed = 4321L

  val OutputCount = 3

  //  val HiddenLayer1Count = 100
  //  val HiddenLayerCount = Seq(HiddenLayer1Count)
  val HiddenLayer1Count = 1000
  val HiddenLayer2Count = 30
  val HiddenLayerCount = Seq(HiddenLayer1Count)

  /** MazeAction */
  object PadAction extends Enumeration {
    val Rest, Left, Right = Value
  }

  object Direction extends Enumeration {
    val NO, NE, SE, SO = Value
  }
  import PadAction._
  import Direction._

  val random = new RandBasis(new MersenneTwister(EnvSeed))

  val endStatus = WallDeepStatus((0, 0), SE, 1)

  /** The state transition map */
  val StatusMap = createMap

  /** Creates a initial game status */
  def initial: WallDeepStatus = {
    val b = (1, random.randInt(Width).get)
    val s = b match {
      case (_, 0) => NE
      case (_, LastCol) => NO
      case _ => random.choose(Seq(Direction.NE, Direction.NO)).get
    }
    val pad = b match {
      case (_, 0) => 0
      case (_, c) if (c >= LastPad) => LastPad
      case (_, c) => c - 1
    }
    WallDeepStatus(b, s, pad)
  }

  /** Creates a initial environment parameters */
  def initEnvParms: (WallDeepStatus, TDParms, TDNeuralNet, TDNeuralNet) = {

    val initStatus = WallDeepStatus.initial

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
      val s0 = WallDeepStatus((Height, 0), dir, pad)
      val s1 = WallDeepStatus((SecondLastRow, 1), SE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx1 =
    validateTx(for {
      pad <- (0 to LastPad)
      dir <- Seq(NO, NE, SE)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((Height, LastCol), dir, pad)
      val s1 = WallDeepStatus((SecondLastRow, SecondLastCol), SO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx2 =
    validateTx(for {
      c <- 1 to SecondLastCol
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((Height, c), NO, pad)
      val s1 = WallDeepStatus((SecondLastRow, c - 1), SO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx3 =
    validateTx(for {
      c <- 1 to SecondLastCol
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((Height, c), NE, pad)
      val s1 = WallDeepStatus((SecondLastRow, c + 1), SE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx4 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((r, 0), NO, pad)
      val s1 = WallDeepStatus((r + 1, 1), NE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx5 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((r, 0), SO, pad)
      val s1 = WallDeepStatus((r - 1, 1), SE, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx6 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((r, LastCol), SE, pad)
      val s1 = WallDeepStatus((r - 1, SecondLastCol), SO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx7 =
    validateTx(for {
      r <- 2 to SecondLastRow
      pad <- (0 to LastPad)
      act <- PadAction.values.toSeq
    } yield {
      val s0 = WallDeepStatus((r, LastCol), NE, pad)
      val s1 = WallDeepStatus((r + 1, SecondLastCol), NO, s0.movePad(act.id))
      ((s0, act), (s1, 0.0))
    })

  private def createTx8 =
    validateTx(for {
      pad <- 1 to SecondLastPad
      c <- pad to pad + 2
    } yield {
      val s0 = WallDeepStatus((1, c), SO, pad)
      val s1 = WallDeepStatus((2, c - 1), NO, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx9 =
    validateTx(for {
      pad <- 1 to SecondLastPad
      c <- pad to pad + 2
    } yield {
      val s0 = WallDeepStatus((1, c), SE, pad)
      val s1 = WallDeepStatus((2, c + 1), NE, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx10 =
    validateTx(for {
      pad <- 0 to LastPad - 2
      c <- pad + 1 to pad + 3
    } yield {
      val s0 = WallDeepStatus((1, c), SO, pad)
      val s1 = WallDeepStatus((2, c - 1), NO, pad + 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx11 =
    validateTx(for {
      pad <- 2 to LastPad
      c <- pad - 1 to pad + 1
    } yield {
      val s0 = WallDeepStatus((1, c), SE, pad)
      val s1 = WallDeepStatus((2, c + 1), NE, pad - 1)
      ((s0, Left), (s1, PositiveReward))
    })

  private def createTx12 =
    validateTx(for {
      dir <- Seq(SO, SE)
      pad <- 0 to 1
    } yield {
      val s0 = WallDeepStatus((1, 0), dir, pad)
      val s1 = WallDeepStatus((2, 1), NE, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx13 =
    validateTx(for {
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallDeepStatus((1, 0), dir, 0)
      val s1 = WallDeepStatus((2, 1), NE, 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx14 =
    validateTx(for {
      pad <- 1 to 2
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallDeepStatus((1, 0), dir, pad)
      val s1 = WallDeepStatus((2, 1), NE, pad - 1)
      ((s0, Left), (s1, PositiveReward))
    })

  private def createTx15 =
    validateTx(for {
      pad <- SecondLastPad to LastPad
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallDeepStatus((1, LastCol), dir, pad)
      val s1 = WallDeepStatus((2, SecondLastCol), NO, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx16 =
    validateTx(for {
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallDeepStatus((1, LastCol), dir, LastPad)
      val s1 = WallDeepStatus((2, SecondLastCol), NO, SecondLastPad)
      ((s0, Left), (s1, PositiveReward))
    })

  private def createTx17 =
    validateTx(for {
      pad <- LastPad - 2 to SecondLastPad
      dir <- Seq(SO, SE)
    } yield {
      val s0 = WallDeepStatus((1, LastCol), dir, pad)
      val s1 = WallDeepStatus((2, SecondLastCol), NO, pad + 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx18 =
    validateTx(for {
      pad <- 2 to LastPad
    } yield {
      val s0 = WallDeepStatus((1, pad - 1), SE, pad)
      val s1 = WallDeepStatus((2, pad - 2), NO, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx19 =
    validateTx(for {
      pad <- 3 to LastPad
    } yield {
      val s0 = WallDeepStatus((1, pad - 2), SE, pad)
      val s1 = WallDeepStatus((2, pad - 3), NO, pad - 1)
      ((s0, Left), (s1, PositiveReward))
    })

  private def createTx20 =
    validateTx(for {
      pad <- 1 to SecondLastPad
    } yield {
      val s0 = WallDeepStatus((1, pad), SE, pad)
      val s1 = WallDeepStatus((2, pad + 1), NE, pad + 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx21 =
    validateTx(for {
      pad <- 0 to LastPad - 2
    } yield {
      val s0 = WallDeepStatus((1, pad + PadSize), SO, pad)
      val s1 = WallDeepStatus((2, pad + PadSize + 1), NE, pad)
      ((s0, Rest), (s1, PositiveReward))
    })

  private def createTx22 =
    validateTx(for {
      pad <- 0 to LastPad - 3
    } yield {
      val s0 = WallDeepStatus((1, pad + PadSize + 1), SO, pad)
      val s1 = WallDeepStatus((2, pad + PadSize + 2), NE, pad + 1)
      ((s0, Right), (s1, PositiveReward))
    })

  private def createTx23 =
    validateTx(for {
      pad <- 1 to SecondLastPad
    } yield {
      val s0 = WallDeepStatus((1, pad + PadSize - 1), SO, pad)
      val s1 = WallDeepStatus((2, pad + PadSize - 2), NO, pad - 1)
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
        createTx15 +:
        createTx16 +:
        createTx17 +:
        createTx18 +:
        createTx19 +:
        createTx20 +:
        createTx21 +:
        createTx22 +:
        createTx23 +:
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

  def normalizeStatus(x: DenseVector[Double]): DenseVector[Double] = Scale :* (x - Mean)
}
