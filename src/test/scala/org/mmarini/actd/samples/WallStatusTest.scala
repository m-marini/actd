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
    c <- Gen.choose(0, Width)
    sr <- Gen.oneOf(-1, 1)
    sc <- Gen.oneOf(-1, 1)
    p <- Gen.choose(0, Width - PadSize)
  } yield WallStatus((r, c), (sr, sc), p)

  property("Initial status vector") {
    Given("initial wall status")
    Then("status vector should be as exepcted")

    forAll((allGen, "s0")) {
      (s0) =>
        val status: DenseVector[Double] = s0.toDenseVector

        val si = s0.ball._1 * Width + s0.ball._2

        for { i <- 0 until Width * Height } {
          if (i == si) {
            status(i) should be(1.0)
          } else {
            status(i) should be(0.0)
          }
        }
//        for { i <- 0 until Width - PadSize } {
//          if (i == s0.pad) {
//            status(i + Width * Height) should be(1.0)
//          } else {
//            status(i + Width * Height) should be(0.0)
//          }
//        }

        if (s0.speed._1 > 0) {
          status(Width * Height + Width - PadSize) should be(1.0)
        } else {
          status(Width * Height + Width - PadSize) should be(0.0)
        }
        if (s0.speed._2 > 0) {
          status(Width * Height + Width - PadSize + 1) should be(1.0)
        } else {
          status(Width * Height + Width - PadSize + 1) should be(0.0)
        }
    }
  }
}
