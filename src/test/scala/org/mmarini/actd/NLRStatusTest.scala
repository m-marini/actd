/**
 *
 */
package org.mmarini.actd

import org.scalatest.FunSpec
import org.scalatest.Matchers
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import scala.math.exp
import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.PropSpec
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen

class NLRStatusTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  val WeightRange = 10.0
  val InputRange = 10.0
  val DeltaRange = 10.0

  val weightGen = Gen.choose(-WeightRange, WeightRange)
  val inGen = Gen.choose(-InputRange, InputRange)
  val deltaGen = Gen.choose(-DeltaRange, DeltaRange).map(DenseVector(_))
  val alphaGen = Gen.choose(0.0, 1.0)
  val inputGen = inGen.map(DenseVector(1.0, _))

  val w1x2Gen = MazeGen.matrix(1, 2, weightGen)
  val w1x3Gen = MazeGen.matrix(1, 3, weightGen)
  val w2x2Gen = MazeGen.matrix(2, 2, weightGen)

  property("bTensor for network 111") {
    Given("a weights tensor for network 1,1,1")
    And("an input vector")
    When("computes bTensor")
    Then("bTensor should be as expected")
    forAll(
      (w1x2Gen, "w0"),
      (w1x2Gen, "w1"),
      (Gen.choose(-InputRange, InputRange), "h01")) {
        (w0, w1, h01) =>
          {
            val h0 = DenseVector(1.0, h01)
            val h11 = 1 / (1 + exp(-(w0(0, 0) + h01 * w0(0, 1))))
            val h21 = w1(0, 0) + h11 * w1(0, 1)

            val h1 = DenseVector(1.0, h11)
            val h2 = DenseVector(1.0, h21)

            val ws = MatrixSeq.create(w0, w1)
            val values = Seq(h0, h1, h2)
            val status = new NLRStatus(ws, values)

            val b = status.bTensor

            b.matrices should have size (1)

            val dhdz = h11 * (1 - h11)

            val b000 = dhdz * w1(0, 1)
            val expected = DenseMatrix((b000))

            TestFuncs.matrixLike(b(0), expected)
          }
      }
  }

  property("bError for network 111") {
    Given("a weights tensor for network 1,1,1")
    And("an input vector")
    And("an delta vector")
    When("computes bError")
    Then("bError should be as expected")
    forAll(
      (w1x2Gen, "w0"),
      (w1x2Gen, "w1"),
      (inGen, "h01"),
      (deltaGen, "delta1")) {
        (w0, w1, h01, delta1) =>
          {
            val h11 = 1 / (1 + exp(-(w0(0, 0) + h01 * w0(0, 1))))
            val h21 = w1(0, 0) + h11 * w1(0, 1)

            val h0 = DenseVector(1.0, h01)
            val h1 = DenseVector(1.0, h11)
            val h2 = DenseVector(1.0, h21)

            val ws = MatrixSeq.create(w0, w1)
            val values = Seq(h0, h1, h2)
            val status = new NLRStatus(ws, values)

            val y = status.bError(delta1)

            y should have size (2)

            val delta0 = delta1(0) * w1(0, 1) * h11 * (1 - h11)
            val expected = DenseVector(delta0)

            TestFuncs.vectorLike(y(0), expected)

            TestFuncs.vectorLike(y(1), delta1)
          }
      }
  }

  property("gradient for network 111") {
    Given("a weights tensor for network 1,1,1")
    And("an input vector")
    And("a delta vector")
    And("an alpha parameter")
    When("computes gradient")
    Then("gradient should be as expected")
    forAll(
      //      (Gen.const(DenseMatrix((1.0, 2.0))), "w0"),
      //      (Gen.const(DenseMatrix((3.0, 4.0))), "w1"),
      //      (Gen.const(1.5), "h01"),
      //      (Gen.const(DenseVector(2.5)), "delta1"),
      //      (Gen.const(0.0), "alpha")) {
      (w1x2Gen, "w0"),
      (w1x2Gen, "w1"),
      (inGen, "h01"),
      (deltaGen, "delta1"),
      (alphaGen, "alpha")) {
        (w0, w1, h01, delta1, alpha) =>
          {
            val h11 = 1 / (1 + exp(-(w0(0, 0) + h01 * w0(0, 1))))
            val h21 = w1(0, 0) + h11 * w1(0, 1)

            val h0 = DenseVector(1.0, h01)
            val h1 = DenseVector(1.0, h11)
            val h2 = DenseVector(1.0, h21)

            val ws = MatrixSeq.create(w0, w1)
            val values = Seq(h0, h1, h2)
            val status = new NLRStatus(ws, values)

            val y = status.gradient(delta1, alpha)

            y should have size (2)

            val grad100 = -(1 - alpha) * delta1(0)
            val grad101 = -(1 - alpha) * delta1(0) * h11 + alpha * w1(0, 1)
            val expected1 = DenseMatrix((grad100, grad101))

            TestFuncs.matrixLike(y.matrices(1), expected1)

            val delta0 = delta1(0) * w1(0, 1) * h11 * (1 - h11)
            val grad000 = -(1 - alpha) * delta0
            val grad001 = -(1 - alpha) * delta0 * h01 + alpha * w0(0, 1)
            val expected = DenseMatrix((grad000, grad001))

            TestFuncs.matrixLike(y.matrices(0), expected)
          }
      }
  }

  property("bTensor for network 121") {
    Given("a weights tensor for network 1,2,1")
    And("an input vector")
    When("computes bTensor")
    Then("bTensor should be as expected")
    forAll(
      (w2x2Gen, "w0"),
      (w1x3Gen, "w1"),
      (inGen, "h0")) {
        (w0, w1, h01) =>
          {
            val h11 = 1 / (1 + exp(-(w0(0, 0) + h01 * w0(0, 1))))
            val h12 = 1 / (1 + exp(-(w0(1, 0) + h01 * w0(1, 1))))
            val h21 = w1(0, 0) + h11 * w1(0, 1) + h12 * w1(0, 2)

            val h0 = DenseVector(1.0, h01)
            val h1 = DenseVector(1.0, h11, h12)
            val h2 = DenseVector(1.0, h21)

            val ws = MatrixSeq.create(w0, w1)
            val values = Seq(h0, h1, h2)
            val status = new NLRStatus(ws, values)

            val b = status.bTensor

            b.matrices should have size (1)

            val b000 = h11 * (1 - h11) * w1(0, 1)
            val b010 = h12 * (1 - h12) * w1(0, 2)
            val expected = DenseMatrix(b000, b010)

            TestFuncs.matrixLike(b(0), expected)
          }
      }
  }

  property("bError for network 121") {
    Given("a weights tensor for network 1,2,1")
    And("an input vector")
    And("an delta vector")
    When("computes bError")
    Then("bError should be as expected")
    forAll(
      (w2x2Gen, "w0"),
      (w1x3Gen, "w1"),
      (inGen, "h01"),
      (deltaGen, "delta1")) {
        (w0, w1, h01, delta1) =>
          {
            val h11 = 1 / (1 + exp(-(w0(0, 0) + h01 * w0(0, 1))))
            val h12 = 1 / (1 + exp(-(w0(1, 0) + h01 * w0(1, 1))))
            val h21 = w1(0, 0) + h11 * w1(0, 1) + h12 * w1(0, 2)

            val h0 = DenseVector(1.0, h01)
            val h1 = DenseVector(1.0, h11, h12)
            val h2 = DenseVector(1.0, h21)

            val ws = MatrixSeq.create(w0, w1)
            val values = Seq(h0, h1, h2)
            val status = new NLRStatus(ws, values)

            val b = status.bError(delta1)

            b should have size (2)

            val b000 = h11 * (1 - h11) * w1(0, 1)
            val b010 = h12 * (1 - h12) * w1(0, 2)
            val expected = DenseVector(
              delta1(0) * b000,
              delta1(0) * b010)

            TestFuncs.vectorLike(b(0), expected)

            TestFuncs.vectorLike(b(1), delta1)
          }
      }
  }

  property("gradient for network 121") {
    Given("a weights tensor for network 1,2,1")
    And("an input vector")
    And("a delta vector")
    And("an alpha parameter")
    When("computes gradient")
    Then("gradient should be as expected")
    forAll(
      //      (Gen.const(DenseMatrix((1.0, 2.0),
      //        (3.0, 4.0))), "w0"),
      //      (Gen.const(DenseMatrix((5.0, 6.0, 7.0))), "w1"),
      //      (Gen.const(1.5), "h01"),
      //      (Gen.const(DenseVector(2.5)), "delta1"),
      //      (Gen.const(0.0), "alpha")) {
      (w2x2Gen, "w0"),
      (w1x3Gen, "w1"),
      (inGen, "h01"),
      (deltaGen, "delta1"),
      (alphaGen, "alpha")) {
        (w0, w1, h01, delta1, alpha) =>
          {
            val h11 = 1 / (1 + exp(-(w0(0, 0) + h01 * w0(0, 1))))
            val h12 = 1 / (1 + exp(-(w0(1, 0) + h01 * w0(1, 1))))
            val h21 = w1(0, 0) + h11 * w1(0, 1) + h12 * w1(0, 2)

            val h0 = DenseVector(1.0, h01)
            val h1 = DenseVector(1.0, h11, h12)
            val h2 = DenseVector(1.0, h21)

            val ws = MatrixSeq.create(w0, w1)
            val values = Seq(h0, h1, h2)
            val status = new NLRStatus(ws, values)

            val g = status.gradient(delta1, alpha)

            g should have size (2)

            val b000 = h11 * (1 - h11) * w1(0, 1)
            val b010 = h12 * (1 - h12) * w1(0, 2)

            val delta00 = delta1(0) * b000
            val delta01 = delta1(0) * b010

            val g100 = -(1 - alpha) * delta1(0)
            val g101 = -(1 - alpha) * delta1(0) * h11 + alpha * w1(0, 1)
            val g102 = -(1 - alpha) * delta1(0) * h12 + alpha * w1(0, 2)
            val e1 = DenseMatrix((g100, g101, g102))

            TestFuncs.matrixLike(g(1), e1)

            val g000 = -(1 - alpha) * delta00
            val g001 = -(1 - alpha) * delta00 * h01 + alpha * w0(0, 1)
            val g010 = -(1 - alpha) * delta01
            val g011 = -(1 - alpha) * delta01 * h01 + alpha * w0(1, 1)
            val expected = DenseMatrix(
              (g000, g001),
              (g010, g011))

            TestFuncs.matrixLike(g(0), expected)
          }
      }
  }
}
