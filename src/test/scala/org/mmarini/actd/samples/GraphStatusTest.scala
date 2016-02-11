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
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import breeze.linalg.DenseVector
import org.mmarini.actd.samples.GraphStatus.MazeAction

class GraphStatusTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  import GraphStatus.MazeAction._

  val MaxWidth = 10
  val MaxHeight = 10

  val sizeGen = for {
    w <- Gen.choose(2, MaxWidth)
    h <- Gen.choose(2, MaxHeight)
  } yield (w, h)

  property("Initial status") {
    Given("initial maze status")
    Then("me location should be 0")

    forAll((sizeGen, "size")) {
      (size) =>
        whenever(size._1 >= 2 && size._2 >= 2) {
          val (w, h) = size
          val maze = GraphStatus.flatFieldMaze(h, w)
          maze should have('s(0))
          maze.size() should be(w * h)
          maze should have('finalStatus(false))
        }
    }
  }

  property("Initial status vector") {
    Given("initial maze status")
    Then("status vector should be as axepcted")

    forAll((sizeGen, "size")) {
      (size) =>
        val (w, h) = size
        val maze = GraphStatus.flatFieldMaze(h, w)

        val status: DenseVector[Double] = maze.toDenseVector

        status should have length (w * h)
        status(0) should be(1.0)
        for {
          i <- 1 until w * h
        } {
          status(i) should be(0.0)
        }
    }
  }

  property("Initial down move") {
    Given("initial maze status")
    When("Apply the down action")
    Then("the feedback should be as axepcted")

    forAll(
      (sizeGen, "size")) {
        (size) =>
          val (w, h) = size
          val maze = GraphStatus.flatFieldMaze(h, w)

          val Feedback(_, _, reward, s1) = maze.apply(Down.id)

          reward should be(-1.0)
          s1 should have('s(w))
          s1 should have('finalStatus(false))
      }
  }

  property("Initial right move") {
    Given("initial maze status")
    When("Apply the down action")
    Then("the feedback should be as axepcted")

    forAll(
      (sizeGen, "size")) {
        (size) =>
          val (w, h) = size
          val maze = GraphStatus.flatFieldMaze(h, w)

          val Feedback(_, _, reward, s1) = maze.apply(Right.id)

          reward should be(-1.0)
          s1 should have('s(1))
          s1 should have('finalStatus(false))
      }
  }

  property("Initial wrong move") {
    Given("initial maze status")
    When("Apply the down action")
    Then("the feedback should be as axepcted")

    forAll(
      (sizeGen, "size"),
      (Gen.oneOf(Up.id, Left.id), "action")) {
        (size, action) =>
          val (w, h) = size
          val maze = GraphStatus.flatFieldMaze(h, w)

          val Feedback(_, _, reward, s1) = maze.apply(action)

          reward should be(-1.0)
          s1 should have('s(0))
          s1 should have('finalStatus(false))
      }
  }

  def end(h: Int, w: Int): GraphStatus = {
    val maze = GraphStatus.flatFieldMaze(h, w)

    val allRight = (1 to w - 1).foldLeft(maze) {
      case (s, _) => s.apply(Right.id).s1.asInstanceOf[GraphStatus]
    }

    val s1 = (1 to h - 1).foldLeft(allRight) {
      case (s, _) => s.apply(Down.id).s1.asInstanceOf[GraphStatus]
    }
    s1
  }

  property("Final status") {
    Given("initial maze status")
    When("Apply the down and right actions for the world size")
    Then("the resulting status should be a final status")

    forAll((sizeGen, "size")) {
      (size) =>
        whenever(size._1 >= 2 && size._2 >= 2) {
          val (w, h) = size
          val s1 = end(w, h)

          s1 should have('s(w * h - 1))
          s1 should have('finalStatus(true))
        }
    }
  }

  property("Next of final status") {
    Given("final maze status")
    When("Apply any actions for the world size")
    Then("the resulting status should be the initial status")

    forAll((sizeGen, "size"),
      (Gen.oneOf(MazeAction.values.toSeq), "action")) {
        (size, action) =>
          whenever(size._1 >= 2 && size._2 >= 2) {
            val (w, h) = size
            val se = end(w, h)
            val Feedback(s0, a, reward, s1) = se.apply(action.id)

            s0 should be(se)
            a should be(action.id)
            reward should be(0.0 +- 1e-6)
            s1 should have('s(0))
            s1 should have('finalStatus(false))
          }
      }
  }
}
