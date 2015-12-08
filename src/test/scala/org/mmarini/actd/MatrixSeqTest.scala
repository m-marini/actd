/**
 *
 */
package org.mmarini.actd

import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.PropSpec
import org.scalacheck.Gen

/**
 * @author us00852
 */
class MatrixSeqTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  val WeightRange = 1e0

  property("flatten") {
    Given("a Weights")
    When("flatten and reshaped")
    Then("it should be equal to the given Weights")
    val wGen = Gen.choose(-WeightRange, WeightRange)
    forAll(
      (MazeGen.weights(
        MazeGen.matrix(2, 3, wGen),
        MazeGen.matrix(1, 3, wGen)), "ws")) {
        (ws) =>
          {
            val data = ws.toDenseVector
            val layers = ws.layers

            val ws1 = MatrixSeq.create(layers, data)

            ws1 should be(ws)
          }
      }
  }
}
