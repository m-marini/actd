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

import org.mmarini.actd.samples.WallStatus.Direction
import org.mmarini.actd.samples.WallStatus.PadAction
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

class WallStatusEndTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import WallStatus._
  import WallStatus.Direction._
  import WallStatus.PadAction._

  /**
   *  2 | O           |
   *  1 |o            |
   *  0 | .===-----===|
   *     0123456789012
   *     0000000000111
   */
  property("end0") {
    forAll((for {
      pad <- Gen.choose(2, LastPad)
      dir <- Gen.oneOf(SE, SW)
    } yield WallStatus(1, 0, dir, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val (_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }

  /**
   *  2 | O           |
   *  1 |o            |
   *  0 | .#===---#===|
   *     0123456789012
   *     0000000000111
   */
  property("end1") {
    forAll((for {
      pad <- Gen.choose(3, LastPad)
      dir <- Gen.oneOf(SE, SW)
    } yield WallStatus(1, 0, dir, pad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val (_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }

  /**
   *  2 | O           |
   *  1 |o            |
   *  0 | .==#----===#|
   *     0123456789012
   *     0000000000111
   */
  property("end2") {
    forAll((for {
      pad <- Gen.choose(1, SecondLastPad)
      dir <- Gen.oneOf(SE, SW)
    } yield WallStatus(1, 0, dir, pad), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val (_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }

  /**
   *  2 |           O |
   *  1 |            o|
   *  0 |===-----===. |
   *     0123456789012
   *     0000000000111
   */
  property("end3") {
    forAll((for {
      pad <- Gen.choose(1, LastPad - 2)
      dir <- Gen.oneOf(SE, SW)
    } yield WallStatus(1, LastCol, dir, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val (_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }

  /**
   *  2 |           O |
   *  1 |            o|
   *  0 |#===----#=== |
   *     0123456789012
   *     0000000000111
   */
  property("end4") {
    forAll((for {
      pad <- Gen.choose(1, SecondLastPad)
      dir <- Gen.oneOf(SE, SW)
    } yield WallStatus(1, LastCol, dir, pad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val (_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }

  /**
   *  2 |           O |
   *  1 |            o|
   *  0 |===#---===#. |
   *     0123456789012
   *     0000000000111
   */
  property("end5") {
    forAll((for {
      pad <- Gen.choose(0, LastPad - 3)
      dir <- Gen.oneOf(SE, SW)
    } yield WallStatus(1, LastCol, dir, pad), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val (_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }
}
