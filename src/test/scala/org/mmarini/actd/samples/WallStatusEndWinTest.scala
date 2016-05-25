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

class WallStatusEndWinTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import WallStatus._
  import WallStatus.Direction._
  import WallStatus.PadAction._

  val EndCount = 1
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
      } yield WallStatus(1, pad + PadSize, SW, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, pad + PadSize, SW, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, pad - 1, SE, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, pad - 1, SE, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, c, SE, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, c, SE, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, c, SW, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, c, SW, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, 0, SW, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, 0, SW, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
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
    forAll((Gen.const(WallStatus(1, 0, SW, 0, EndCount)), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, LastCol, SE, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Rest.id)
          s1 should have('finalStatus(true))
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
      } yield WallStatus(1, LastCol, SE, pad, EndCount), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Right.id)
          s1 should have('finalStatus(true))
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
    forAll((Gen.const(WallStatus(1, LastCol, SE, LastPad, EndCount)), "s0")) {
      (s0) =>
        {
          val (_, _, r, s1) = s0.apply(Left.id)
          s1 should have('finalStatus(true))
          r should be(PositiveReward)
        }
    }
  }
}
