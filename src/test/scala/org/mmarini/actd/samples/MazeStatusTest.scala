/**
 *
 */
package org.mmarini.actd.samples

import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import CellStatus.Me
import CellStatus.Target
import CellStatus.Wall
import MazeStatus.WorldHeight
import MazeStatus.WorldWidth
import MazeStatus.WorldSize
import MazeAction._
import breeze.linalg.DenseVector
import org.mmarini.actd.Feedback

class MazeStatusTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  val locGen = Gen.choose(0, WorldSize - 1)

  property("Status vector") {
    Given("a Me location")
    And("a valid target location")
    And("a wall location")
    And("the related maze status")
    Then("status vector should be as axepcted")

    forAll(
      (locGen, "me"),
      (locGen, "target"),
      (locGen, "wall")) {
        (me, target, wall) =>
          whenever(me != target && me != wall && target != wall) {

            val maze = MazeStatus(WorldHeight, WorldWidth, me, target, Set(wall))

            val status: DenseVector[Double] = maze.toDenseVector

            status should have length (WorldSize * (CellStatus.values.size))
            status(me) should be(1.0)
            status(target + WorldSize) should be(1.0)
            status(wall + 2 * WorldSize) should be(1.0)
            for {
              i <- 0 until WorldSize
            } {
              if (i != me)
                status(i) should be(0.0)
              if (i != target)
                status(i + WorldSize) should be(0.0)
              if (i != wall)
                status(i + 2 * WorldSize) should be(0.0)
            }
          }
      }
  }

  def move(action: MazeAction.Value)(loc: Int): Option[Int] = {
    val row = loc / WorldWidth
    val col = loc % WorldWidth
    action match {
      case Up if (row > 0) => Some(loc - WorldWidth)
      case Down if (row < WorldHeight - 1) => Some(loc + WorldWidth)
      case Left if (col > 0) => Some(loc - 1)
      case Right if (col < WorldWidth - 1) => Some(loc + 1)
      case _ => None
    }
  }

  property("Normal move action") {
    Given("a my location")
    And("a target location not equal to my location")
    And("the related maze status")
    And("a valid action")
    When("apply the action")
    Then("cell @ Me location should be empty")
    And("cell near location should be Me")
    forAll(
      (locGen, "me"),
      (Gen.oneOf(MazeAction.values.toSeq), "action"),
      (locGen, "target")) {
        //      (Gen.const(2), "me"),
        //      (Gen.const(MazeAction.Right), "action"),
        //      (Gen.const(2), "target")) {
        (me, action, target) =>
          whenever(me != target && !move(action)(me).isEmpty) {

            val maze0 = MazeStatus(WorldHeight, WorldWidth, me, target, Set())
            val me1 = move(action)(me).get

            val Feedback(_, _, reward, maze1) = maze0(action.id)
            val endEpisode = me1 == target
            val expReward = if (me1 == target) 1.0 else -1.0

            maze1 should have('me(me1))
            maze1 should have('endEpisode(endEpisode))
            maze1.asInstanceOf[MazeStatus].status(me1) should contain(Me)
            maze1.asInstanceOf[MazeStatus].status(me) should not contain (Me)

            reward should be(expReward)
          }
      }
  }

  property("Bad move action") {
    Given("a my location")
    And("a target location")
    And("the related maze status")
    And("a invalid action")
    When("apply the action")
    Then("next status should be the initial status")

    val borderGen = for {
      r <- Gen.oneOf(0, WorldHeight - 1)
      c <- Gen.oneOf(0, WorldWidth - 1)
    } yield r * WorldWidth + c

    forAll(
      (borderGen, "me"),
      (Gen.oneOf(MazeAction.values.toSeq), "action"),
      (locGen, "target")) {
        (me, action, target) =>
          whenever(me != target && move(action)(me).isEmpty) {

            val init = MazeStatus(WorldHeight, WorldWidth, me, target, Set())

            val Feedback(_, _, reward, maze1) = init(action.id)

            maze1 should be(init)
            reward should be(-1.0)
          }
      }
  }

  property("move action to target") {
    Given("a Me location")
    And("a valid action")
    And("a valid target location ")
    And("the related maze status")
    When("apply the action")
    Then("cell @ Me location should be empty")
    And("cell near location should be Me")
    And("next status should be an end episode")
    forAll(
      (locGen, "me"),
      (Gen.oneOf(MazeAction.values.toSeq), "action")) {
        (me, action) =>
          whenever(!move(action)(me).isEmpty) {
            val target = move(action)(me).get

            val init = MazeStatus(WorldHeight, WorldWidth, me, target, Set())

            val Feedback(_, _, reward, maze1) = init(action.id)

            maze1 should have('me(target))
            maze1 should have('target(target))
            maze1 should have('endEpisode(true))
            reward should be(1.0)
          }
      }
  }

  property("end episode") {
    Given("a Me location == target location")
    And("any action")
    And("the related maze status")
    When("apply the action")
    Then("next status should be init")
    And("reward should be 0.0")
    forAll(
      (locGen, "me"),
      (Gen.oneOf(MazeAction.values.toSeq), "action")) {
        (me, action) =>
          {
            val init = MazeStatus(WorldHeight, WorldWidth, me, me, Set())

            val Feedback(_, _, reward, maze1) = init(action.id)

            maze1 should be(MazeStatus.init)
            reward should be(0.0)
          }
      }
  }

  property("Initial status") {
    Given("initial status")
    Then("cell @0,0 should be Me")
    And("cell @H,W should be Target")
    forAll(
      (Gen.const(MazeStatus.init), "maze")) {
        (maze) =>
          {
            maze should have(
              'rows(WorldHeight),
              'cols(WorldWidth),
              'me(0),
              'target(WorldSize - 1))
          }
      }
  }

  property("apply for empty") {
    Given("initial status")
    Then("any cell other then @0,0 or @4,4 should be Empty")
    forAll(
      (Gen.const(MazeStatus.init), "maze"),
      (locGen, "loc")) {
        (maze, loc) =>
          {
            whenever(loc != 0 && loc != WorldSize - 1) {
              maze.status(loc) shouldBe empty
            }
          }
      }
  }
}
