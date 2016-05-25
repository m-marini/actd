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
import org.mmarini.actd.samples.WallStatus.Direction
import org.mmarini.actd.samples.WallStatus.PadAction
import org.mmarini.actd.samples.WallStatus.TransitionMapper

import com.typesafe.scalalogging.LazyLogging

import breeze.stats.distributions.RandBasis

/** The status of wall game */
case class WallStatus(row: Int, col: Int, direction: Direction.Value, pad: Int, countdown: Int) {

  import WallStatus._
  import Direction._
  import PadAction._

  require(col >= 0)
  require(col < Width)
  require(pad >= 0)
  require(pad <= LastPad)
  require(row <= Height)
  require(row >= 0)
  require(row >= 1 || row == 0 && direction == SE && pad == 1, s"$row, $col $direction $pad")

  /** Moves the pad by action */
  private def movePad(action: PadAction.Value): WallStatus = action match {
    case Left => if (pad > 0) WallStatus(row, col, direction, pad - 1, countdown) else this
    case Right => if (pad < LastPad) WallStatus(row, col, direction, pad + 1, countdown) else this
    case _ => this
  }

  /** Produce the feedback of an applied action */
  def apply(action: Action): (WallStatus, Action, Double, WallStatus) = next(action) match {
    case (s1, r) => (this, action, r, s1)
  }

  private def direction(dir: Direction.Value): WallStatus = if (dir != direction) WallStatus(row, col, dir, pad, countdown) else this

  private def moveBall(dir: Direction.Value): WallStatus = direction(dir).moveBall

  private def moveBall: WallStatus = this match {
    case WallStatus(Height, 0, _, _, _) => direction(SE).moveBallNoBounce
    case WallStatus(Height, LastCol, _, _, _) => direction(SW).moveBallNoBounce
    case WallStatus(Height, _, NE, _, _) => direction(SE).moveBallNoBounce
    case WallStatus(Height, _, NW, _, _) => direction(SW).moveBallNoBounce
    case WallStatus(_, 0, NW, _, _) => direction(NE).moveBallNoBounce
    case WallStatus(_, 0, SW, _, _) => direction(SE).moveBallNoBounce
    case WallStatus(_, LastCol, NE, _, _) => direction(NW).moveBallNoBounce
    case WallStatus(_, LastCol, SE, _v, _) => direction(SW).moveBallNoBounce
    case _ => moveBallNoBounce
  }

  private def moveBallNoBounce = direction match {
    case NE => WallStatus(row + 1, col + 1, direction, pad, countdown)
    case SE => WallStatus(row - 1, col + 1, direction, pad, countdown)
    case NW => WallStatus(row + 1, col - 1, direction, pad, countdown)
    case _ => WallStatus(row - 1, col - 1, direction, pad, countdown)
  }

  /** Produce the feedback of an applied action */
  private def next(action: Action): (WallStatus, Double) =
    if (finalStatus) {
      (WallStatus.initial, 0.0)
    } else if (row == 1) {
      if (direction == NE || direction == NW) {
        (moveBall.movePad(PadAction(action)), 0.0)
      } else {
        // Ball on bottom
        PadAction(action) match {
          case Rest => restOnBottom(this)
          case Right => rightOnBottom(this)
          case Left => leftOnBottom(this)
        }
      }
    } else {
      (moveBall.movePad(PadAction(action)), 0.0)
    }

  private def bounce: WallStatus = if (countdown <= 1) endStatus else WallStatus(row, col, direction, pad, countdown - 1)

  /** Returns true if is a final status */
  def finalStatus: Boolean = this == endStatus
}

/** A factory of [[WallStatus1]] */
object WallStatus extends LazyLogging {

  type TransitionSource = (WallStatus, PadAction.Value)
  type TransitionTarget = (WallStatus, Double)
  type TransitionMap = Map[TransitionSource, TransitionTarget]

  object PadAction extends Enumeration {
    val Rest, Left, Right = Value
  }
  object Direction extends Enumeration {
    val NW, NE, SE, SW = Value
  }

  val Height = 10
  val Width = 14
  val PadSize = 3
  val SecondLastRow = Height - 1
  val PositiveReward = 5.0
  val NegativeReward = -1.0
  val LastCol = Width - 1
  val SecondLastCol = Width - 2
  val LastPad = Width - PadSize
  val SecondLastPad = LastPad - 1
  val Countdown = 10

  val EnvSeed = 4321L

  import Direction._
  import PadAction._

  val envRandom = new RandBasis(new MersenneTwister(EnvSeed))

  val endStatus = WallStatus(0, 0, SE, 1, 0)

  /** Creates a initial game status */
  def initial: WallStatus = {
    val col = envRandom.randInt(Width).get
    val s = col match {
      case 0 => NE
      case LastCol => NW
      case _ => envRandom.choose(Seq(Direction.NE, Direction.NW)).get
    }
    val pad = col match {
      case 0 => 0
      case c if (c >= LastPad) => LastPad
      case c => c - 1
    }
    WallStatus(1, col, s, pad, Countdown)
  }

  type TransitionMapper = PartialFunction[WallStatus, (WallStatus, Double)]

  private def rightOnBottom = rightOnBottom1 orElse
    rightOnBottom2 orElse
    rightOnBottom3

  private def rightOnBottom1: TransitionMapper = (s) => s match {
    /*
       *   . |
       *    o|
       *  ===|
       *
       *   . |
       *    o|
       * ===#|
       *
       *    . |
       *     o|
       * ===# |
       */
    case WallStatus(_, LastCol, _, pad, _) if (pad >= LastPad - 2) => (s.moveBall(NW).movePad(Right).bounce, PositiveReward)
    /*
       *     . |
       *      o|
       * ===#. |
       */
    case WallStatus(_, LastCol, _, _, _) => (endStatus, NegativeReward)
    /*
       * | .
       * |o
       * |===#
       */
    case WallStatus(_, 0, _, 0, _) => (s.moveBall(NE).movePad(Right).bounce, PositiveReward)
    /*
       * |o
       * | ===#
       */
    case WallStatus(_, 0, _, _, _) => (endStatus, NegativeReward)
  }

  private def rightOnBottom2: TransitionMapper = (s) => s match {
    /*
       * o
       *  ===#
       *
       *    o
       * ===#-
       */
    case WallStatus(_, col, SE, pad, _) if (col < pad || col >= pad + PadSize) => (endStatus, NegativeReward)
    /*
       *   o
       *  .===#
       *
       *      o
       * ===#-
       */
    case WallStatus(_, col, SW, pad, _) if (col <= pad || col >= pad + PadSize + 2) => (endStatus, NegativeReward)
    /*
       *      .
       *     o
       * ===#
       */
    case WallStatus(_, col, SW, pad, _) if (col == pad + PadSize + 1) => (s.moveBall(NE).movePad(Right).bounce, PositiveReward)
  }

  private def rightOnBottom3: TransitionMapper = (s) => s match {
    /*
       *   . O
       *    o
       * -==#
       *
       *  . O
       *   o
       * -==#
       *
       * . O
       *  o
       * -==#
       */
    case WallStatus(_, col, SW, _, _) => (s.moveBall(NW).movePad(Right).bounce, PositiveReward)
    /*
       * O .
       *  o
       * ===#
       */
    case WallStatus(_, col, SE, _, _) => (s.moveBall(NE).movePad(Right).bounce, PositiveReward)
  }

  private def leftOnBottom = leftOnBottom1 orElse
    leftOnBottom2 orElse
    leftOnBottom3

  private def leftOnBottom1: TransitionMapper = (s) => s match {
    /*
       * | .
       * |o
       * |===
       *
       * | .
       * |o
       * |#===
       *
       * | .
       * |o
       * | #===
       */
    case WallStatus(_, 0, _, pad, _) if (pad <= 2) => (s.moveBall(NE).movePad(Left).bounce, PositiveReward)
    /*
       * |o
       * | .#===
       */
    case WallStatus(_, 0, _, _, _) => (endStatus, NegativeReward)
    /*
       *   . |
       *    o|
       * #==-|
       */
    case WallStatus(_, LastCol, _, LastPad, _) => (s.moveBall(NW).movePad(Left).bounce, PositiveReward)
    /*
       *     o|
       * #==- |
       */
    case WallStatus(_, LastCol, _, _, _) => (endStatus, NegativeReward)
  }

  private def leftOnBottom2: TransitionMapper = (s) => s match {
    /*
       * o
       *  .#==-
       *
       *    o
       * #==-
       */
    case WallStatus(_, col, SE, pad, _) if (col <= pad - 3 || col >= pad + PadSize - 1) => (endStatus, NegativeReward)
    /*
       *  o
       * .#==-
       *
       *     o
       * #==-
       */
    case WallStatus(_, col, SW, pad, _) if (col < pad || col >= pad + PadSize) => (endStatus, NegativeReward)
    /*
       * .
       *  o
       *   #==-
       */
    case WallStatus(_, col, SE, pad, _) if (col == pad - 2) => (s.moveBall(NW).movePad(Left).bounce, PositiveReward)
  }

  private def leftOnBottom3: TransitionMapper = (s) => s match {
    /*
       * O .
       *  o
       *  #==-
       *
       * O .
       *  o
       *  #==-
       *
       *  O .
       *   o
       * #==-
       */
    case WallStatus(_, _, SE, _, _) => (s.moveBall(NE).movePad(Left).bounce, PositiveReward)
    /*
       * . O
       *  o
       * #==-
       */
    case WallStatus(_, _, SW, _, _) => (s.moveBall(NW).movePad(Left).bounce, PositiveReward)
  }

  private def restOnBottom = restOnBottom1 orElse
    restOnBottom2 orElse
    restOnBottom3

  private def restOnBottom1: TransitionMapper = (s) => s match {
    /*
       *   . |
       *    o|
       *  ===|
       *
       *   . |
       *    o|
       * === |
       */
    case WallStatus(_, LastCol, _, pad, _) if (pad >= SecondLastPad) => (s.moveBall(NW).bounce, PositiveReward)
    /*
       *   . |
       *    o|
       *  ===|
       *
       *   . |
       *    o|
       * === |
       */
    case WallStatus(_, LastCol, _, pad, _) if (pad >= SecondLastPad) => (s.moveBall(NW).bounce, PositiveReward)
    /*
       *     o|
       * ===. |
       */
    case WallStatus(_, LastCol, _, _, _) => (endStatus, NegativeReward)
    /*
       * | .
       * |o
       * |===
       *
       * | .
       * |o
       * | ===
       */
    case WallStatus(_, 0, _, pad, _) if (pad <= 1) => (s.moveBall(NE).bounce, PositiveReward)
  }

  private def restOnBottom2: TransitionMapper = (s) => s match {
    /*
       * |o
       * | .===
       */
    case WallStatus(_, 0, _, _, _) => (endStatus, NegativeReward)
    /*
       * o
       *  .===
       *
       *    o
       * === .
       */
    case WallStatus(_, c, SE, pad, _) if (c >= pad + PadSize || c <= pad - 2) => (endStatus, NegativeReward)
    /*
       *  o
       * . ===
       *
       *     o
       * ===.
       */
    case WallStatus(_, c, SW, pad, _) if (c >= pad + PadSize + 1 || c <= pad - 1) => (endStatus, NegativeReward)
  }

  private def restOnBottom3: TransitionMapper = (s) => s match {
    /*
       * .
       *  o
       *   ===
       */
    case WallStatus(_, col, SE, pad, _) if (col == pad - 1) => (s.moveBall(NW).bounce, PositiveReward)
    /*
       *  .
       * o
       * ===
       */
    case WallStatus(_, _, SE, _, _) => (s.moveBall(NE).bounce, PositiveReward)
    /*
       *     .
       *    o
       * ===
       */
    case WallStatus(_, col, SW, pad, _) if (col == pad + PadSize) => (s.moveBall(NE).bounce, PositiveReward)
    /*
       *  . O
       *   o
       * ===
       */
    case WallStatus(_, _, SW, _, _) => (s.moveBall(NW).bounce, PositiveReward)
  }
}
