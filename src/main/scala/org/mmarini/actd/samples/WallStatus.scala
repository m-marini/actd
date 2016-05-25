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

import com.typesafe.scalalogging.LazyLogging

import breeze.stats.distributions.RandBasis

/** The status of wall game */
case class WallStatus(row: Int, col: Int, direction: Direction.Value, pad: Int) {

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

  /** Returns a [[WallStatus1]] with changed pad location */
  def pad(x: Int): WallStatus = WallStatus(row, col, direction, x)

  /** Moves the pad by action */
  private def movePad(action: Action) = PadAction.apply(action) match {
    case Left if pad > 0 => pad - 1
    case Right if pad < LastPad => pad + 1
    case _ => pad
  }

  /** Produce the feedback of an applied action */
  def apply(action: Action): (WallStatus, Action, Double, WallStatus) = apply1(action) match {
    case (s1, r) => (this, action, r, s1)
  }

  /** Produce the feedback of an applied action */
  private def apply1(action: Action): (WallStatus, Double) =
    if (finalStatus) {
      (WallStatus.initial, 0.0)
    } else if (row == Height) {
      // Ball on top
      ballOnTop match {
        case (nc, nd) => (WallStatus(Height - 1, nc, nd, movePad(action)), 0.0)
      }
    } else if (row == 1) {
      // Ball on bottom
      PadAction(action) match {
        case Rest => restOnBottom
        case Right => rightOnBottom
        case Left => leftOnBottom
      }
    } else {
      ballOnMiddle match {
        case (nr, nc, nd) => (WallStatus(nr, nc, nd, movePad(action)), 0.0)
      }
    }

  private def leftOnBottom =
    this match {
      case WallStatus(_, _, NW, _) => (WallStatus(2, col - 1, NW, movePad((Left.id))), 0.0)
      case WallStatus(_, _, NE, _) => (WallStatus(2, col + 1, NE, movePad((Left.id))), 0.0)
      /*
       * | .
       * |o
       * |===
       */
      case WallStatus(_, 0, _, 0) => (WallStatus(2, 1, NE, 0), PositiveReward)
      /*
       * | .
       * |o
       * |#===
       */
      case WallStatus(_, 0, _, 1) => (WallStatus(2, 1, NE, 0), PositiveReward)
      /*
       * | .
       * |o
       * | #===
       */
      case WallStatus(_, 0, _, 2) => (WallStatus(2, 1, NE, 1), PositiveReward)
      /*
       * |o
       * | .#===
       */
      case WallStatus(_, 0, _, _) => (endStatus, NegativeReward)
      /*
       *   . |
       *    o|
       * #==-|
       */
      case WallStatus(_, LastCol, _, LastPad) => (WallStatus(2, SecondLastCol, NW, SecondLastPad), PositiveReward)
      /*
       *     o|
       * #==- |
       */
      case WallStatus(_, LastCol, _, _) => (endStatus, NegativeReward)
      /*
       * o
       *  .#==-
       *
       *    o
       * #==-
       */
      case WallStatus(_, col, SE, pad) if (col <= pad - 3 || col >= pad + PadSize - 1) => (endStatus, NegativeReward)
      /*
       *  o
       * .#==-
       *
       *     o
       * #==-
       */
      case WallStatus(_, col, SW, pad) if (col < pad || col >= pad + PadSize) => (endStatus, NegativeReward)
      /*
       * .
       *  o
       *   #==-
       */
      case WallStatus(_, col, SE, pad) if (col == pad - 2) => (WallStatus(2, col - 1, NW, movePad(Left.id)), PositiveReward)
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
      case WallStatus(_, col, SE, _) => (WallStatus(2, col + 1, NE, movePad(Left.id)), PositiveReward)
      /*
       * . O
       *  o
       * #==-
       */
      case WallStatus(_, col, SW, _) => (WallStatus(2, col - 1, NW, movePad(Left.id)), PositiveReward)
    }

  private def rightOnBottom =
    this match {
      case WallStatus(_, _, NW, _) => (WallStatus(2, col - 1, NW, movePad((Left.id))), 0.0)
      case WallStatus(_, _, NE, _) => (WallStatus(2, col + 1, NE, movePad((Left.id))), 0.0)
      /*
       *   . |
       *    o|
       *  ===|
       */
      case WallStatus(_, LastCol, _, LastPad) => (WallStatus(2, SecondLastCol, NW, pad), PositiveReward)
      /*
       *   . |
       *    o|
       * ===#|
       */
      case WallStatus(_, LastCol, _, SecondLastPad) => (WallStatus(2, SecondLastCol, NW, LastPad), PositiveReward)
      /*
       *    . |
       *     o|
       * ===# |
       */
      case WallStatus(_, LastCol, _, pad) if (pad == LastPad - 2) => (WallStatus(2, SecondLastCol, NW, movePad(Right.id)), PositiveReward)
      /*
       *     . |
       *      o|
       * ===#. |
       */
      case WallStatus(_, LastCol, _, _) => (endStatus, NegativeReward)
      /*
       * | .
       * |o
       * |===#
       */
      case WallStatus(_, 0, _, 0) => (WallStatus(2, 1, NE, movePad(Right.id)), PositiveReward)
      /*
       * |o
       * | ===#
       */
      case WallStatus(_, 0, _, _) => (endStatus, NegativeReward)
      /*
       * o
       *  ===#
       *
       *    o
       * ===#-
       */
      case WallStatus(_, col, SE, pad) if (col < pad || col >= pad + PadSize) => (endStatus, NegativeReward)
      /*
       *   o
       *  .===#
       *
       *      o
       * ===#-
       */
      case WallStatus(_, col, SW, pad) if (col <= pad || col >= pad + PadSize + 2) => (endStatus, NegativeReward)
      /*
       *      .
       *     o
       * ===#
       */
      case WallStatus(_, col, SW, pad) if (col == pad + PadSize + 1) => (WallStatus(2, col + 1, NE, movePad(Right.id)), PositiveReward)
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
      case WallStatus(_, col, SW, _) => (WallStatus(2, col - 1, NW, movePad(Right.id)), PositiveReward)
      /*
       * O .
       *  o
       * ===#
       */
      case WallStatus(_, col, SE, _) => (WallStatus(2, col + 1, NE, movePad(Right.id)), PositiveReward)
    }

  private def restOnBottom =
    this match {
      case WallStatus(_, _, NW, _) => (WallStatus(2, col - 1, NW, movePad((Left.id))), 0.0)
      case WallStatus(_, _, NE, _) => (WallStatus(2, col + 1, NE, movePad((Left.id))), 0.0)
      /*
       *   . |
       *    o|
       *  ===|
       */
      case WallStatus(_, LastCol, _, LastPad) => (WallStatus(2, SecondLastCol, NW, pad), PositiveReward)
      /*
       *   . |
       *    o|
       * === |
       */
      case WallStatus(_, LastCol, _, SecondLastPad) => (WallStatus(2, SecondLastCol, NW, pad), PositiveReward)
      /*
       *     o|
       * ===. |
       */
      case WallStatus(_, LastCol, _, _) => (endStatus, NegativeReward)
      /*
       * | .
       * |o
       * |===
       */
      case WallStatus(_, 0, _, 0) => (WallStatus(2, 1, NE, pad), PositiveReward)
      /*
       * | .
       * |o
       * | ===
       */
      case WallStatus(_, 0, _, 1) => (WallStatus(2, 1, NE, pad), PositiveReward)
      /*
       * |o
       * | .===
       */
      case WallStatus(_, 0, _, _) => (endStatus, NegativeReward)
      /*
       * o
       *  .===
       *
       *    o
       * === .
       */
      case WallStatus(_, c, SE, _) if (c >= pad + PadSize || c <= pad - 2) =>
        (endStatus, NegativeReward)
      /*
       *  o
       * . ===
       *
       *     o
       * ===.
       */
      case WallStatus(_, c, SW, _) if (c >= pad + PadSize + 1 || c <= pad - 1) =>
        (endStatus, NegativeReward)
      /*
       * .
       *  o
       *   ===
       */
      case WallStatus(_, col, SE, pad) if (col == pad - 1) => (WallStatus(2, col - 1, NW, pad), PositiveReward)
      /*
       *  .
       * o
       * ===
       */
      case WallStatus(_, _, SE, _) => (WallStatus(2, col + 1, NE, pad), PositiveReward)
      /*
       *     .
       *    o
       * ===
       */
      case WallStatus(_, _col, SW, pad) if (col == pad + PadSize) => (WallStatus(2, col + 1, NE, pad), PositiveReward)
      /*
       *  . O
       *   o
       * ===
       */
      case WallStatus(_, _, SW, _) => (WallStatus(2, col - 1, NW, pad), PositiveReward)
    }

  private def ballOnMiddle =
    this match {
      case WallStatus(r, 0, NW, _) => (r + 1, 1, NE)
      case WallStatus(r, 0, SW, _) => (r - 1, 1, SE)
      case WallStatus(r, LastCol, NE, _) => (r + 1, SecondLastCol, NW)
      case WallStatus(r, LastCol, SE, _) => (r - 1, SecondLastCol, SW)
      case WallStatus(r, c, NE, _) => (r + 1, c + 1, NE)
      case WallStatus(r, c, SE, _) => (r - 1, c + 1, SE)
      case WallStatus(r, c, NW, _) => (r + 1, c - 1, NW)
      case WallStatus(r, c, SW, _) => (r - 1, c - 1, SW)
    }

  private def ballOnTop =
    this match {
      case WallStatus(_, 0, _, _) => (1, SE)
      case WallStatus(_, LastCol, _, _) => (SecondLastCol, SW)
      case WallStatus(_, c, NE, _) => (c + 1, SE)
      case WallStatus(_, c, SE, _) => (c + 1, SE)
      case WallStatus(_, c, NW, _) => (c - 1, SW)
      case WallStatus(_, c, SW, _) => (c - 1, SW)
    }

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

  val EnvSeed = 4321L

  import Direction._
  import PadAction._

  val envRandom = new RandBasis(new MersenneTwister(EnvSeed))

  val endStatus = WallStatus(0, 0, SE, 1)

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
    WallStatus(1, col, s, pad)
  }

}
