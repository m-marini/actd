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

import org.mmarini.actd.Feedback
import org.mmarini.actd.samples.WallStatus.Direction
import org.mmarini.actd.samples.WallStatus.PadAction
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import com.sun.org.apache.xalan.internal.xsltc.compiler.LastCall

class WallStatusTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import WallStatus.PadAction._
  import WallStatus.Direction._
  import WallStatus._

  /**
   *    ====
   * 10 |o
   *  9 | .
   *
   *     012
   *     000
   */
  property("tx0") {
    forAll((for {
      pad <- Gen.choose(0, LastPad)
      dir <- Gen.oneOf(Direction.values.toSeq)
    } yield WallStatus((Height, 0), dir, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            r should be(0.0)
            s1 should matchPattern {
              case WallStatus((SecondLastRow, 1), SE, _) =>
            }
          }
      }
  }

  /**
   *    ====
   * 10   o|
   *  9  . |
   *
   *    012
   *    111
   */
  property("tx1") {
    forAll((for {
      pad <- Gen.choose(0, LastPad)
      dir <- Gen.oneOf(Direction.values.toSeq)
    } yield WallStatus((Height, LastCol), dir, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((SecondLastRow, SecondLastCol), SO, _) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *    ===============
   * 10 | o         o |
   *  9 |. O       . O|
   *
   *     0123456789012
   *     0000000000111
   */
  property("tx2") {
    forAll((for {
      c <- Gen.choose(1, LastCol - 1)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((Height, c), NO, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((SecondLastRow, c), SO, _) if (c == s0.ball._2 - 1) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *    ===============
   * 10 | o         o |
   *  9 |O .       O .|
   *
   *     0123456789012
   *     0000000000111
   */
  property("tx3") {
    forAll((for {
      c <- Gen.choose(1, LastCol - 1)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((Height, c), NE, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((SecondLastRow, c), SE, _) if (c == s0.ball._2 + 1) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *    =======
   * 10 | O
   *  9 |o
   *  8 | .
   *
   *  3 | O
   *  2 |o
   *  1 | .
   *  0  ===
   *     012
   */
  property("tx4") {
    forAll((for {
      r <- Gen.choose(2, SecondLastRow)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, 0), SO, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((row, 1), SE, _) if (row == s0.ball._1 - 1) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *    =======
   * 10 | .
   *  9 |o
   *  8 | O
   *
   *  3 | .
   *  2 |o
   *  1 | O
   *  0  ===
   *     012
   */
  property("tx5") {
    forAll((for {
      r <- Gen.choose(2, SecondLastRow)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, 0), NO, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((row, 1), NE, _) if (row == s0.ball._1 + 1) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *    ====
   * 10  O |
   *  9   o|
   *  8  . |
   *
   *  3  O |
   *  2   o|
   *  2  . |
   *  0 ===
   *    012
   *    111
   */
  property("tx6") {
    forAll((for {
      r <- Gen.choose(2, SecondLastRow)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, LastCol), SE, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((row, SecondLastCol), SO, _) if (row == s0.ball._1 - 1) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *    ====
   * 10  . |
   *  9   o|
   *  8  O |
   *
   *  3  . |
   *  2   o|
   *  1  O |
   *  0 ===
   *    012
   *    111
   */
  property("tx7") {
    forAll((for {
      r <- Gen.choose(2, SecondLastRow)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, LastCol), NE, pad), "s0"),
      (Gen.oneOf(PadAction.values.toSeq), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((row, SecondLastCol), NO, _) if (row == s0.ball._1 + 1) =>
            }
            r should be(0.0)
          }
      }
  }

  /**
   *  2 ..OOO---..OOO|
   *  1  ooo-----ooo |
   *  0  ===-----=== |
   *    0123456789012
   *    0000000000111
   */
  property("tx8") {
    forAll((for {
      pad <- Gen.choose(1, SecondLastPad)
      c <- Gen.choose(pad, pad + 2)
    } yield WallStatus((1, c), SO, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, c), NO, p) if (c == s0.ball._2 - 1 && p == s0.pad) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 OOO..---OOO..|
   *  1  ooo-----ooo |
   *  0  ===-----=== |
   *    0123456789012
   *    0000000000111
   */
  property("tx9") {
    forAll((for {
      pad <- Gen.choose(1, SecondLastPad)
      c <- Gen.choose(pad, pad + 2)
    } yield WallStatus((1, c), SE, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, c), NE, p) if (c == s0.ball._2 + 1 && p == s0.pad) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 ..OOO---..OOO|
   *  1  ooo-----ooo |
   *  0 ===#----===# |
   *    0123456789012
   *    0000000000111
   */
  property("tx10") {
    forAll((for {
      pad <- Gen.choose(0, LastPad - 2)
      c <- Gen.choose(pad + 1, pad + 3)
    } yield WallStatus((1, c), SO, pad), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, c), NO, p) if (c == s0.ball._2 - 1 && p == s0.pad + 1) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 OOO..---OOO..|
   *  1  ooo-----ooo |
   *  0  #===----#===|
   *    0123456789012
   *    0000000000111
   */
  property("tx11") {
    forAll((for {
      pad <- Gen.choose(2, LastPad)
      c <- Gen.choose(pad - 1, pad + 1)
    } yield WallStatus((1, c), SE, pad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, c), NE, p) if (c == s0.ball._2 + 1 && p == s0.pad - 1) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2  .
   *  1 o
   *  0 ====
   *    0123456789012
   *    0000000000111
   */
  property("tx12") {
    forAll((for {
      dir <- Gen.oneOf(SE, SO)
      pad <- Gen.choose(0, 1)
    } yield WallStatus((1, 0), dir, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, 1), NE, p) if (p == s0.pad) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2  .
   *  1 o
   *  0 ===#
   *    0123456789012
   *    0000000000111
   */
  property("tx13") {
    forAll((for {
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, 0), dir, 0), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, 1), NE, 1) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 | .
   *  1 |o
   *  0 |##===
   *     0123456789012
   *     0000000000111
   */
  property("tx14") {
    forAll((for {
      pad <- Gen.choose(1, 2)
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, 0), dir, pad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, 1), NE, p) if (p == s0.pad - 1) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 |           . |
   *  1 |            o|
   *  0 |         ===-|
   *     0123456789012
   *     0000000000111
   */
  property("tx15") {
    forAll((for {
      dir <- Gen.oneOf(SE, SO)
      pad <- Gen.choose(SecondLastPad, LastPad)
    } yield WallStatus((1, LastCol), dir, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, SecondLastCol), NO, p) if (p == s0.pad) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 |           . |
   *  1 |            o|
   *  0 |         #===|
   *     0123456789012
   *     0000000000111
   */
  property("tx16") {
    forAll((for {
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, LastCol), dir, LastPad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, SecondLastCol), NO, SecondLastPad) =>
            }
            r should be(PositiveReward)
          }
      }
  }

  /**
   *  2 |           . |
   *  1 |            o|
   *  0 |        ===##|
   *     0123456789012
   *     0000000000111
   */
  property("tx17") {
    forAll((for {
      pad <- Gen.choose(LastPad - 2, SecondLastPad)
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, LastCol), dir, pad), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should matchPattern {
              case WallStatus((2, SecondLastCol), NO, p) if (p == s0.pad + 1) =>
            }
            r should be(PositiveReward)
          }
      }
  }

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
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, 0), dir, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
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
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, 0), dir, pad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
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
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, 0), dir, pad), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
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
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, LastCol), dir, pad), "s0"),
      (Gen.const(Rest), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
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
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, LastCol), dir, pad), "s0"),
      (Gen.const(Left), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
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
      dir <- Gen.oneOf(SE, SO)
    } yield WallStatus((1, LastCol), dir, pad), "s0"),
      (Gen.const(Right), "act")) {
        (s0, act) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(act.id)
            s1 should have('finalStatus(true))
            r should be(NegativeReward)
          }
      }
  }
}
