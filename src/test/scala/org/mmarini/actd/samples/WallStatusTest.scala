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

class WallStatusTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import WallStatus.PadAction._
  import WallStatus.Direction._
  import WallStatus._

  /**
   *    ====
   * 10 |o
   *  9 | O
   *
   *     012
   *     000
   */
  property("Ball in (10,0) SE") {
    forAll((for {
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((Height - 1, 1), NO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((Height, 0), SE, _) =>
          }
        }
    }
  }

  /**
   *    ====
   * 10   o|
   *  9  O |
   *
   *    012
   *    111
   */
  property("Ball in (10,12) SO") {
    forAll((for {
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((9, 11), NE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((Height, LastCol), SO, _) =>
          }
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
  property("Ball in (9,3-12) NO") {
    forAll((for {
      c <- Gen.choose(3, LastCol)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((Height - 1, c), NO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((Height, c), SO, _) if (c == s0.ball._2 - 1) =>
          }
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
  property("Ball in (9,0-10) NE") {
    forAll((for {
      c <- Gen.choose(0, Width - 3)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((Height - 1, c), NE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((Height, c), SE, _) if (c == s0.ball._2 + 1) =>
          }
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
  property("Ball in (2-9,0) SE") {
    forAll((for {
      r <- Gen.choose(3, Height)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, 1), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((row, 0), SE, _) if (row == s0.ball._1 - 1) =>
          }
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
  property("Ball in (2-9,0) NE") {
    forAll((for {
      r <- Gen.choose(1, Height - 2)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, 1), NO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((row, 0), NE, _) if (row == s0.ball._1 + 1) =>
          }
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
  property("Ball in (2-9,12) SO") {
    forAll((for {
      r <- Gen.choose(3, Height)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, LastCol - 1), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((row, LastCol), SO, _) if (row == s0.ball._1 - 1) =>
          }
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
  property("Ball in (2-9,12) NO") {
    forAll((for {
      r <- Gen.choose(1, Height - 2)
      pad <- Gen.choose(0, LastPad)
    } yield WallStatus((r, 11), NE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((row, LastCol), NO, _) if (row == s0.ball._1 + 1) =>
          }
        }
    }
  }

  /**
   * 2 | O
   * 1 |o
   * 0  ===
   *    012
   */
  property("Ball in (1,0) NE bouncing") {
    forAll((Gen.const(WallStatus((2, 1), SO, 0)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, 0), NE, _) =>
          }
        }
    }
  }

  /**
   * 2 |O .          |
   * 1 | o           |
   * 0  -===         |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,1) NE bouncing") {
    forAll((for { pad <- Gen.choose(0, 1) } yield WallStatus((2, 0), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, 1), NE, _) =>
          }
        }
    }
  }

  /**
   * 2 | O .     O . |
   * 1 |  o       o  |
   * 0  --===-----===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,2-10) NE bouncing") {
    forAll((for {
      col <- Gen.choose(1, SecondLastPad)
      pad <- Gen.choose(col - 1, col + 1)
    } yield WallStatus((2, col), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, c), NE, _) if (c == s0.ball._2 + 1) =>
          }
        }
    }
  }

  /**
   * 2 |          O .|
   * 1 |           o |
   * 0           -===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,11) NE bouncing") {
    forAll((for {
      pad <- Gen.choose(SecondLastPad, LastPad)
    } yield WallStatus((2, Width - 3), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, SecondLastCol), NE, _) =>
          }
        }
    }
  }

  /**
   * 2 |           O |
   * 1 |            o|
   * 0            ===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,12) NO bouncing") {
    forAll((Gen.const(WallStatus((2, 11), SE, LastPad)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, LastCol), NO, _) =>
          }
        }
    }
  }

  /**
   * 2 |. O          |
   * 1 | o           |
   * 0  -===         |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,1) NO bouncing") {
    forAll((for { pad <- Gen.choose(0, 1) } yield WallStatus((2, 2), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, 1), NO, _) =>
          }
        }
    }
  }

  /**
   * 2 | . O     . O |
   * 1 |  o       o  |
   * 0  --===-----===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,2-10) NO bouncing") {
    forAll((for {
      c <- Gen.choose(3, LastPad + 1)
      pad <- Gen.choose(c - 3, c - 1)
    } yield WallStatus((2, c), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, c), NO, _) if (c == s0.ball._2 - 1) =>
          }
        }
    }
  }

  /**
   * 2 |          . O|
   * 1 |           o |
   * 0           -===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,11) NO bouncing") {
    forAll((for {
      pad <- Gen.choose(SecondLastPad, LastPad)
    } yield WallStatus((2, LastCol), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, SecondLastCol), NO, _) =>
          }
        }
    }
  }

  /**
   * 2 |    O
   * 1 |   o
   * 0  ===
   *    01234
   */
  property("Ball in (1,3) NE bouncing") {
    forAll((Gen.const(WallStatus((2, 4), SO, 0)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, 3), NE, _) =>
          }
        }
    }
  }

  /**
   * 2 O    |
   * 1  o   |
   * 0   ===
   *   89012
   *   00111
   */
  property("Ball in (1,9) NO bouncing ") {
    forAll((Gen.const(WallStatus((2, 8), SE, LastPad)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(PositiveReward)
          s1 should matchPattern {
            case WallStatus((1, SecondLastPad), NO, _) =>
          }
        }
    }
  }

  /**
   * 2 |  O-------O  |
   * 1 |   o-------o |
   * 0 |===#.-------.|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,3-11) SE") {
    forAll((
      for {
        c <- Gen.choose(3, LastCol - 1)
      } yield WallStatus((1, c), SE, c - PadSize), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Right.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |  O-------O  |
   * 1 | o-------o   |
   * 0 |.-------.#===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,1-9) SO") {
    forAll((
      for {
        c <- Gen.choose(1, LastPad - 1)
      } yield WallStatus((1, c), SO, c + 1), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Left.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |             |
   * 1 |o-------o    |
   * 0 | O-------O===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,0-8) SE missing pad") {
    forAll((
      for {
        c <- Gen.choose(0, LastPad - 2)
        pad <- Gen.choose(c + 2, LastPad)
      } yield WallStatus((1, c), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |             |
   * 1 |   o......o  |
   * 0 |=== O------O |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,3-10) SE missing pad") {
    forAll((
      for {
        c <- Gen.choose(3, LastCol - 2)
        pad <- Gen.choose(0, c - 3)
      } yield WallStatus((1, c), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |             |
   * 1 |           O |
   * 0 |===-----=== o|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,11) SE missing pad") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 2)
      } yield WallStatus((1, LastCol - 1), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |           O |
   * 1 |            o|
   * 0 |===------=== |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (2,11) SE") {
    forAll((
      for {
        pad <- Gen.choose(0, LastPad - 1)
      } yield WallStatus((2, LastCol - 1), SE, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((1, LastCol), SO, _) =>
          }
        }
    }
  }

  /**
   * 2 | O           |
   * 1 |o            |
   * 0 | ===------===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (2,1) SO") {
    forAll((
      for {
        pad <- Gen.choose(1, LastPad)
      } yield WallStatus((2, 1), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(0.0)
          s1 should matchPattern {
            case WallStatus((1, 0), SE, _) =>
          }
        }
    }
  }

  /**
   * 2 |             |
   * 1 |  o------o   |
   * 0 | O------O ===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,2-9) SO missing pad") {
    forAll((
      for {
        c <- Gen.choose(2, SecondLastPad)
        pad <- Gen.choose(c + 1, LastPad)
      } yield WallStatus((1, c), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |             |
   * 1 | O           |
   * 0 |o ===-----===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,1) SO missing pad") {
    forAll((
      for {
        pad <- Gen.choose(2, LastPad)
      } yield WallStatus((1, 1), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |            |
   * 1 |    o------o|
   * 0 |===O------O |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,4-12) SO missing pad") {
    forAll((
      for {
        c <- Gen.choose(PadSize + 1, LastCol)
        pad <- Gen.choose(0, c - 4)
      } yield WallStatus((1, c), SO, pad), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)
          r should be(NegativeReward)
          s1 should have('finalStatus(true))
        }
    }
  }

  /**
   * 2 |     O-----O |
   * 1 |    o-----o  |
   * 0 |===#-----#   |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,4-10) SO pad R") {
    forAll((
      for {
        c <- Gen.choose(PadSize + 1, LastCol - 2)
      } yield WallStatus((1, c), SO, c - 4), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Right.id)
          s1 should matchPattern {
            case WallStatus((2, c), NE, _) if (c == s0.ball._2 + 1) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   * 2 | O-----O     |
   * 1 |  o-----o    |
   * 0 |   #-----#===|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,2-8) SE pad L") {
    forAll((
      for {
        c <- Gen.choose(2, LastPad - 2)
      } yield WallStatus((1, c), SE, c + 2), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Left.id)
          s1 should matchPattern {
            case WallStatus((2, c), NO, _) if (c == s0.ball._2 - 1) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   * 2 |O
   * 1 | o
   * 0 |  #===
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,1) SE pad L") {
    forAll((Gen.const(WallStatus((1, 1), SE, 3)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Left.id)
          s1 should matchPattern {
            case WallStatus((2, 0), NE, 2) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   * 2 | O
   * 1 |o
   * 0 | #===
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,0) SE pad L") {
    forAll((Gen.const(WallStatus((1, 0), SE, 2)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Left.id)
          s1 should matchPattern {
            case WallStatus((2, 1), NE, 1) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   * 2 |            O|
   * 1 |           o |
   * 0 |       ===#  |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,11) SO pad R") {
    forAll((Gen.const(WallStatus((1, LastCol - 1), SO, LastPad - 3)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Right.id)
          s1 should matchPattern {
            case WallStatus((2, LastCol), NO, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   * 2 |           O |
   * 1 |            o|
   * 0 |        ===# |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,12) SO pad L") {
    forAll((Gen.const(WallStatus((1, LastCol), SO, LastPad - 2)), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Right.id)
          s1 should matchPattern {
            case WallStatus((2, SecondLastCol), NO, _) =>
          }
          r should be(PositiveReward)
        }
    }
  }

  /**
   * 1 |    o-------o|
   * 0 |#==*-------* |
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,4-12) SO pad L") {
    forAll((
      for {
        c <- Gen.choose(PadSize + 1, LastCol)
      } yield WallStatus((1, c), SO, c - 3), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Left.id)
          s1 should have('finalStatus(true))
          r should be(NegativeReward)
        }
    }
  }

  /**
   * 1 |o-------o    |
   * 0 | *-------*==#|
   *    0123456789012
   *    0000000000111
   */
  property("Ball in (1,0-8) SE pad R") {
    forAll((
      for {
        c <- Gen.choose(0, LastPad - 2)
      } yield WallStatus((1, c), SE, c + 2), "s0")) {
      s0 =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Right.id)
          s1 should have('finalStatus(true))
          r should be(NegativeReward)
        }
    }
  }
}
