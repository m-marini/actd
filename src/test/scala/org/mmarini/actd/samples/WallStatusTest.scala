/**
 *
 */
package org.mmarini.actd.samples

import org.mmarini.actd.Feedback
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import breeze.linalg.DenseVector
import org.mmarini.actd.samples.GraphStatus.MazeAction

class WallStatusTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import WallStatus.PadAction._
  import WallStatus._

  val allGen = for {
    r <- Gen.choose(0, Height)
    c <- Gen.choose(0, Width - 1)
    sr <- Gen.oneOf(-1, 1)
    sc <- Gen.oneOf(-1, 1)
    p <- Gen.choose(0, Width - PadSize)
  } yield WallStatus((r, c), (sr, sc), p)

  val finalGen = for {
    c <- Gen.choose(0, Width - 1)
    sr <- Gen.oneOf(-1, 1)
    sc <- Gen.oneOf(-1, 1)
    p <- Gen.choose(0, Width - PadSize)
  } yield WallStatus((Height, c), (sr, sc), p)

  val actionGen = Gen.choose(0, 2)

  property("Initial status vector") {
    Given("initial wall status")
    Then("status vector should be as exepcted")

    forAll((allGen, "s0")) {
      (s0) =>
        val IndexesSize4 = 4

        val status: DenseVector[Double] = s0.toDenseVector

        status should have('size(Width * (Height + 1) + Width - PadSize + 3))

        val i0 = status.findAll(_ == 0.0)
        val i1 = status.findAll(_ == 1.0)

        val si = s0.ball._1 * Width + s0.ball._2

        i1 should contain(si)
        i1 should contain(s0.pad + Width * (Height + 1))
        if (s0.speed._1 > 0) {
          i1 should contain(Width * (Height + 1) + Width - PadSize + 1)
        }
        if (s0.speed._2 > 0) {
          i1 should contain(Width * (Height + 1) + Width - PadSize + 2)
        }

        if (s0.speed._1 > 0) {
          if (s0.speed._2 > 0) {
            i1 should have('size(IndexesSize4))
          } else {
            i1 should have('size(3))
          }
        } else if (s0.speed._2 > 0) {
          i1 should have('size(3))
        } else {
          i1 should have('size(2))
        }
    }
  }

  property("final status") {
    Given("status with ball in Hieght row")
    Then("status should be final")
    forAll((finalGen, "s0")) {
      (s0) =>
        {
          s0 should have('finalStatus(true))
        }
    }
  }

  property("restart status") {
    Given("status with ball in Hieght row")
    And("an action")
    When("apply action")
    Then("result status should be inital status")
    forAll(
      (finalGen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1.asInstanceOf[WallStatus].ball._1 should be(Height - 1)
            s1.asInstanceOf[WallStatus].speed._1 should be(-1)

            val col = s1.asInstanceOf[WallStatus].ball._2
            val expPad = col match {
              case 0 => 0
              case c if (c - 1 >= Width - PadSize) => Width - PadSize
              case c => c - 1
            }
            s1.asInstanceOf[WallStatus].pad should be(expPad)

          }
      }
  }

  property("left bounce") {
    Given("status with ball in Hieght-1 row and col not beside limits")
    And("ball at left pad side")
    When("apply action rest")
    Then("result status should be as expected")

    val s0Gen = for {
      c <- Gen.choose(1, Width - 3)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((Height - 1, c), (1, sc), c)

    forAll((s0Gen, "s0")) {
      (s0) =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)

          r should be(1.0)
          s1 should have('ball((Height - 2, s0.ball._2 - 1)))
          s1 should have('speed((-1, -1)))
        }
    }
  }

  property("center bounce") {
    Given("status with ball in Hieght-1 row and col not beside limits")
    And("ball at center pad side")
    When("apply action rest")
    Then("result status should be as expected")

    val s0Gen = for {
      c <- Gen.choose(1, Width - 2)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((Height - 1, c), (1, sc), c - 1)

    forAll((s0Gen, "s0")) {
      (s0) =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)

          r should be(1.0)
          s1 should have('ball((Height - 2, s0.ball._2 + s0.speed._2)))
          s1 should have('speed((-1, s0.speed._2)))
        }
    }
  }

  property("right bounce") {
    Given("status with ball in Hieght-1 row and col not beside limits")
    And("ball at right pad side")
    When("apply action rest")
    Then("result status should be as expected")

    val s0Gen = for {
      c <- Gen.choose(2, Width - 2)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((Height - 1, c), (1, sc), c - 2)

    forAll((s0Gen, "s0")) {
      (s0) =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)

          r should be(1.0)
          s1 should have('ball((Height - 2, s0.ball._2 + 1)))
          s1 should have('speed((-1, 1)))
        }
    }
  }

  property("all left bounce") {
    Given("status with ball in Hieght-1 row and col not beside limits")
    And("ball at 0 column")
    When("apply action rest")
    Then("result status should be as expected")

    val s0Gen = for {
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((Height - 1, 0), (1, sc), 0)

    forAll((s0Gen, "s0")) {
      (s0) =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)

          r should be(1.0)
          s1 should have('ball((Height - 2, 1)))
          s1 should have('speed((-1, 1)))
        }
    }
  }

  property("all right bounce") {
    Given("status with ball in Hieght-1 row and col not beside limits")
    And("ball at 39 column")
    When("apply action rest")
    Then("result status should be as expected")

    val s0Gen = for {
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((Height - 1, Width - 1), (1, sc), Width - PadSize)

    forAll((s0Gen, "s0")) {
      (s0) =>
        {
          val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)

          r should be(1.0)
          s1 should have('ball((Height - 2, Width - 2)))
          s1 should have('speed((-1, -1)))
        }
    }
  }

  property("no bounce") {
    Given("status with ball in Hieght-1 row")
    And("pad not in range")
    When("apply action rest")
    Then("result status should be as expected")

    val s0Gen = for {
      c <- Gen.choose(1, Width - 2)
      pad <- Gen.choose(1, Width - PadSize)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((Height - 1, c), (1, sc), pad)

    forAll((s0Gen, "s0")) {
      (s0) =>
        whenever(s0.ball._2 < s0.pad || s0.ball._2 > s0.pad + 2) {
          {
            val Feedback(_, _, r, s1) = s0.apply(WallStatus.PadAction.Rest.id)

            r should be(NegativeReward)
            s1 should have('finalStatus(true))
          }
        }
    }
  }

  property("flow") {
    Given("status with ball not in boundary")
    When("apply any action")
    Then("result status should be as expected")

    val s0Gen = for {
      r <- Gen.choose(1, Height - 2)
      c <- Gen.choose(1, Width - 2)
      pad <- Gen.choose(0, Width - PadSize)
      sr <- Gen.oneOf(-1, 1)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((r, c), (sr, sc), pad)

    forAll((s0Gen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1 should have('ball((s0.ball._1 + s0.speed._1, s0.ball._2 + s0.speed._2)))
            s1 should have('speed(s0.speed))
          }
      }
  }

  property("left wall bounce") {
    Given("status with ball at left wall")
    When("apply any action")
    Then("result status should be as expected")

    val s0Gen = for {
      r <- Gen.choose(1, Height - 2)
      pad <- Gen.choose(0, Width - PadSize)
      sr <- Gen.oneOf(-1, 1)
    } yield WallStatus((r, 0), (sr, -1), pad)

    forAll((s0Gen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1 should have('ball((s0.ball._1 + s0.speed._1, 1)))
            s1 should have('speed((s0.speed._1, 1)))
          }
      }
  }

  property("right wall bounce") {
    Given("status with ball at right wall")
    When("apply any action")
    Then("result status should be as expected")

    val s0Gen = for {
      r <- Gen.choose(1, Height - 2)
      pad <- Gen.choose(0, Width - PadSize)
      sr <- Gen.oneOf(-1, 1)
    } yield WallStatus((r, Width - 1), (sr, 1), pad)

    forAll((s0Gen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1 should have('ball((s0.ball._1 + s0.speed._1, Width - 2)))
            s1 should have('speed((s0.speed._1, -1)))
          }
      }
  }

  property("top wall bounce") {
    Given("status with ball at top wall")
    When("apply any action")
    Then("result status should be as expected")

    val s0Gen = for {
      c <- Gen.choose(1, Width - 2)
      pad <- Gen.choose(0, Width - PadSize)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((0, c), (-1, sc), pad)

    forAll((s0Gen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1 should have('ball((1, s0.ball._2 + s0.speed._2)))
            s1 should have('speed((1, s0.speed._2)))
          }
      }
  }

  property("top left wall bounce") {
    Given("status with ball at top left wall")
    When("apply any action")
    Then("result status should be as expected")

    val s0Gen = for {
      pad <- Gen.choose(0, Width - PadSize)
      sr <- Gen.oneOf(-1, 1)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((0, 0), (sr, sc), pad)

    forAll((s0Gen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1 should have('ball((1, 1)))
            s1 should have('speed((1, 1)))
          }
      }
  }

  property("top right wall bounce") {
    Given("status with ball at top right wall")
    When("apply any action")
    Then("result status should be as expected")

    val s0Gen = for {
      pad <- Gen.choose(0, Width - PadSize)
      sr <- Gen.oneOf(-1, 1)
      sc <- Gen.oneOf(-1, 1)
    } yield WallStatus((0, Width - 1), (sr, sc), pad)

    forAll((s0Gen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          {
            val Feedback(_, _, r, s1) = s0.apply(a)

            r should be(0.0)
            s1 should have('ball((1, Width - 2)))
            s1 should have('speed((1, -1)))
          }
      }
  }

  property("pad move") {
    Given("a non final status")
    When("apply any action")
    Then("result status should be as expected")

    forAll((allGen, "s0"),
      (actionGen, "a")) {
        (s0, a) =>
          whenever(!s0.finalStatus) {
            val Feedback(_, _, _, s1) = s0.apply(a)
            (PadAction.apply(a), s0.pad) match {
              case (PadAction.Left, 0) => s1 should have('pad(0))
              case (PadAction.Left, p) => s1 should have('pad(p - 1))
              case (PadAction.Right, p) if (p == Width - PadSize) => s1 should have('pad(Width - PadSize))
              case (PadAction.Right, p) => s1 should have('pad(p + 1))
              case (PadAction.Rest, p) => s1 should have('pad(p))
            }
          }
      }
  }
}
