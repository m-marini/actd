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

import scala.math.max
import scala.math.min

import org.mmarini.actd.samples.WallStatus.Direction
import org.mmarini.actd.samples.WallStatus.PadAction
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

class WallStatusExtraTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import WallStatus._
  import WallStatus.Direction._
  import WallStatus.PadAction._

  /**
   *  9 | o----------o |
   *  3 | o----------o |
   *  ...
   *  3 | o----------o |
   *  2 | o----------o |
   *  1 |              |
   *     01234567890123
   */
  property("ball on middle, action any, run") {
    forAll((
      for {
        r <- Gen.choose(2, SecondLastRow)
        c <- Gen.choose(1, SecondLastCol)
        dir <- Gen.oneOf(Direction.values.toSeq)
        pad <- Gen.choose(0, LastPad)
      } yield WallStatus(r, c, dir, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "a")) {
        (s0, a) =>
          {
            val (_, _, r, s1) = s0.apply(a.id)

            val (er, ec) = s0.direction match {
              case NE => (s0.row + 1, s0.col + 1)
              case SE => (s0.row - 1, s0.col + 1)
              case NW => (s0.row + 1, s0.col - 1)
              case SW => (s0.row - 1, s0.col - 1)
            }
            val ep = a match {
              case Rest => s0.pad
              case Right => min(s0.pad + 1, LastPad)
              case Left => max(s0.pad - 1, 0)
            }
            s1 should have('row(er))
            s1 should have('col(ec))
            r should be(0.0)
          }
      }
  }

  /**
   *  2 |.           . |
   *  1 | o           o|
   *  0 |      ===     |
   *     01234567890123
   */
  property("ball no on first row, action any, run") {
    forAll((
      for {
        c <- Gen.choose(1, LastCol)
        pad <- Gen.choose(0, LastPad)
      } yield WallStatus(1, c, NW, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "a")) {
        (s0, a) =>
          {
            val (_, _, r, s1) = s0.apply(a.id)
            s1 should matchPattern {
              case WallStatus(2, _, NW, _) =>
            }
            s1 should have('col(s0.col - 1))
            r should be(0.0)
          }
      }
  }

  /**
   *  2 | .           .|
   *  1 |o           o |
   *  0 |      ===     |
   *     01234567890123
   */
  property("ball ne on first row, action any, run") {
    forAll((
      for {
        c <- Gen.choose(0, SecondLastCol)
        pad <- Gen.choose(0, LastPad)
      } yield WallStatus(1, c, NE, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "a")) {
        (s0, a) =>
          {
            val (_, _, r, s1) = s0.apply(a.id)
            s1 should matchPattern {
              case WallStatus(2, _, NE, _) =>
            }
            s1 should have('col(s0.col + 1))
            r should be(0.0)
          }
      }
  }

  /**
   *  2 |O      O     |
   *  1 |o-------o    |
   *  0   .===    .===|
   *     0123456789012
   */
  property("ball se left pad, action rest, lose") {
    forAll((
      for {
        pad <- Gen.choose(2, LastPad)
        c <- Gen.choose(0, pad - 2)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |       O     |
   *  1 |o-------o    |
   *  0   ===#    ===#|
   *     0123456789012
   */
  property("ball se left pad, action right, lose") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(0, pad - 1)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |      O      |
   *  1 |o------o     |
   *  0   .#===  .#===|
   *     0123456789012
   */
  property("ball se left pad, action left, lose") {
    forAll((
      for {
        pad <- Gen.choose(3, LastPad)
        c <- Gen.choose(0, pad - 3)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |  O        O  |
   *  1 | o--------o   |
   *  0  . ===   . ===|
   *     0123456789012
   */
  property("ball sw left pad, action rest, lose") {
    forAll((
      for {
        pad <- Gen.choose(2, LastPad)
        c <- Gen.choose(1, pad - 1)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |  O       O  |
   *  1 | o-------o   |
   *  0  .===#   .===#|
   *     0123456789012
   */
  property("ball sw left pad, action right, lose") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(1, pad)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |  O      O   |
   *  1 | o------o    |
   *  0  . #=== . #===|
   *     0123456789012
   */
  property("ball sw left pad, action left, lose") {
    forAll((
      for {
        pad <- Gen.choose(3, LastPad)
        c <- Gen.choose(1, pad - 2)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |  O       O  |
   *  1 |   o-------o |
   *  0  === .   === .
   *     0123456789012
   */
  property("ball se right pad, action rest, lose") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
        c <- Gen.choose(pad + PadSize, SecondLastCol)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |   O       O  |
   *  1 |    o-------o |
   *  0  ===# .  ===# .
   *     0123456789012
   */
  property("ball se right pad, action right, lose") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
        c <- Gen.choose(pad + PadSize + 1, SecondLastCol)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 | O       O  |
   *  1 |  o-------o |
   *  0  #==..   #==-.
   *     0123456789012
   */
  property("ball se right pad, action left, lose") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 2)
        c <- Gen.choose(pad + PadSize, SecondLastCol)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |     O       |
   *  1 |    o-------o|
   *  0  ===.    ===. |
   *     0123456789012
   */
  property("ball sw right pad, action rest, lose") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
        c <- Gen.choose(pad + PadSize + 1, LastCol)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |      O      |
   *  1 |     o------o|
   *  0  ===#.  ===#. |
   *     0123456789012
   */
  property("ball sw right pad, action right, lose") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
        c <- Gen.choose(pad + PadSize + 2, LastCol)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  2 |     O       |
   *  1 |    o-------o|
   *  0 |#==-    #==- |
   *     0123456789012
   */
  property("ball sw right pad, action left, lose") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(pad + PadSize, LastCol)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
          s1.finalStatus should be(true)
          r should be(NegativeReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |    .       .|
   *  1 |   o       o |
   *  0  ===     ===
   *     0123456789012
   */
  property("ball sw right pad, action rest, bounce") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
      } yield WallStatus(1, pad + PadSize, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should matchPattern {
            case WallStatus(2, _, NE, _) =>
          }
          s1.col should be(s0.col + 1)
          s1.pad should be(s0.pad)
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |  . O     . O|
   *  1 |   o       o |
   *  0  ===#    ===#
   *     0123456789012
   */
  property("ball sw right pad, action right, bounce") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
      } yield WallStatus(1, pad + PadSize, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1.col should be(s0.col - 1)
          s1.pad should be(s0.pad + 1)
          s1 should matchPattern {
            case WallStatus(2, _, NW, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |O .    O .  |
   *  1 | o      o   |
   *  0   #===   #===
   *     0123456789012
   */
  property("ball se left pad, action left, bounce") {
    forAll((
      for {
        pad <- Gen.choose(2, LastPad)
      } yield WallStatus(1, pad - 1, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1.col should be(s0.col + 1)
          s1.pad should be(s0.pad - 1)
          s1 should matchPattern {
            case WallStatus(2, _, NE, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |.      .    |
   *  1 | o      o   |
   *  0    ===    ===
   *     0123456789012
   */
  property("ball se left pad, action rest, bounce") {
    forAll((
      for {
        pad <- Gen.choose(2, LastPad)
      } yield WallStatus(1, pad - 1, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should matchPattern {
            case WallStatus(2, _, NW, _) =>
          }
          s1.col should be(s0.col - 1)
          s1.pad should be(s0.pad)
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |OOO .   OOO .|
   *  1 | ooo     ooo |
   *  0   ===     ===
   *     0123456789012
   */
  property("ball se over pad, action rest, bounce") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(pad, pad + PadSize - 1)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1.col should be(s0.col + 1)
          s1.pad should be(s0.pad)
          s1 should matchPattern {
            case WallStatus(2, _, NE, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |OOO .   OOO .|
   *  1 | ooo     ooo |
   *  0   ===#    ===#
   *     0123456789012
   */
  property("ball se over pad, action right, bounce") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(pad, pad + PadSize - 1)
      } yield WallStatus(1, c, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1.col should be(s0.col + 1)
          s1.pad should be(s0.pad + 1)
          s1 should matchPattern {
            case WallStatus(2, _, NE, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |. OOO   . OOO|
   *  1 | ooo     ooo |
   *  0   ===     ===
   *     0123456789012
   */
  property("ball sw over pad, action rest, bounce") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(pad, pad + PadSize - 1)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1.col should be(s0.col - 1)
          s1.pad should be(s0.pad)
          s1 should matchPattern {
            case WallStatus(2, _, NW, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 |OOO .   OOO .|
   *  1 | ooo     ooo |
   *  0  #===    #===
   *     0123456789012
   */
  property("ball sw over pad, action left, bounce") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad - 1)
        c <- Gen.choose(pad, pad + PadSize - 1)
      } yield WallStatus(1, c, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1.col should be(s0.col - 1)
          s1.pad should be(s0.pad - 1)
          s1 should matchPattern {
            case WallStatus(2, _, NW, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 | .
   *  1 |o
   *  0  ===
   *     0123456789012
   *
   *  3 |
   *  2 | .
   *  1 |o
   *  0   ===
   *     0123456789012
   *
   */
  property("ball sw angle, action rest, bounce") {
    forAll((
      for {
        pad <- Gen.choose(0, 1)
      } yield WallStatus(1, 0, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1.pad should be(s0.pad)
          s1 should matchPattern {
            case WallStatus(2, 1, NE, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 | .
   *  1 |o
   *  0  #===
   *     0123456789012
   *
   *  3 |
   *  2 | .
   *  1 |o
   *  0   #===
   *     0123456789012
   *
   */
  property("ball sw angle, action left, bounce") {
    forAll((
      for {
        pad <- Gen.choose(1, 2)
      } yield WallStatus(1, 0, SW, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1.pad should be(s0.pad - 1)
          s1 should matchPattern {
            case WallStatus(2, 1, NE, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  3 |
   *  2 | .
   *  1 |o
   *  0  ===#
   *     0123456789012
   */
  property("ball sw angle, action right, bounce") {
    forAll((
      Gen.const(WallStatus(1, 0, SW, 0)), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1.pad should be(s0.pad + 1)
          s1 should matchPattern {
            case WallStatus(2, 1, NE, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  2 |           . |
   *  1 |            o|
   *  0            ===
   *     0123456789012
   *
   *  2 |           . |
   *  1 |            o|
   *  0           ===
   *     0123456789012
   */
  property("ball se angle, action rest, bounce") {
    forAll((
      for {
        pad <- Gen.choose(LastPad - 1, LastPad)
      } yield WallStatus(1, LastCol, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1.pad should be(s0.pad)
          s1 should matchPattern {
            case WallStatus(2, SecondLastCol, NW, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  2 |           . |
   *  1 |            o|
   *  0           ===#
   *     0123456789012
   *
   *  2 |           . |
   *  1 |            o|
   *  0          ===#
   *     0123456789012
   */
  property("ball se angle, action right, bounce") {
    forAll((
      for {
        pad <- Gen.choose(LastPad - 2, LastPad - 1)
      } yield WallStatus(1, LastCol, SE, pad), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1.pad should be(s0.pad + 1)
          s1 should matchPattern {
            case WallStatus(2, SecondLastCol, NW, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   *  2 |           . |
   *  1 |            o|
   *  0           #===
   *     0123456789012
   */
  property("ball se angle, action left, bounce") {
    forAll((
      Gen.const(WallStatus(1, LastCol, SE, LastPad)), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should matchPattern {
            case WallStatus(2, SecondLastCol, NW, SecondLastPad) =>
          }
          r should be(PositiveReward)
        }
    }
  }
}
