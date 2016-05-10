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

package org.mmarini.actd

import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks

import breeze.linalg.DenseMatrix
import scala.math.abs
import scala.math.signum

/**
 * @author us00852
 */
class CostFunctionTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  property("cost") {
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)

    val errGen = MazeGen.vector(n, 1)
    //    val l1Gen = Gen.const(0.0)
    //    val l2Gen = Gen.const(0.0)
    val l1Gen = Gen.choose(0.0, 1e3)
    val l2Gen = Gen.choose(0.0, 1e3)

    forAll(
      (l1Gen, "l1"),
      (l2Gen, "l2"),
      (errGen, "err"),
      (wGen, "w")) {
        (l1, l2, err, w) =>
          {
            val func = CostFunction.elasticNet(l1, l2)
            val j = func(err, w)
            val exp = (err(0) * err(0) +
              err(1) * err(1) +
              l2 *
              (w(0, 1) * w(0, 1) +
                w(0, 2) * w(0, 2) +
                w(1, 1) * w(1, 1) +
                w(1, 2) * w(1, 2))) / 2 +
                l1 *
                (abs(w(0, 1)) +
                  abs(w(0, 2)) +
                  abs(w(1, 1)) +
                  abs(w(1, 2)))
            j shouldBe exp +- 1e-6
          }
      }
  }

  property("gradIdent") {
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)
    val xGen = MazeGen.vector(m - 1, 10.0)

    val errGen = MazeGen.vector(n, 1e-3)
    val hGen = MazeGen.vector(n, 1)
    val l1Gen = Gen.choose(0.0, 1e3)
    val l2Gen = Gen.choose(0.0, 1e3)

    forAll(
      (l1Gen, "l1"),
      (l2Gen, "l2"),
      (errGen, "err"),
      (hGen, "h"),
      (xGen, "x"),
      (wGen, "w")) {
        (l1, l2, err, h, x, w) =>
          {
            val func = CostFunction.elasticNet(l1, l2)
            val y = h + err
            val dj = func.grad(err, Ident.grad(h, x), x, w)

            val dj00 = -err(0)
            val dj01 = -err(0) * x(0) + l2 * w(0, 1) + l1 * signum(w(0, 1))
            val dj02 = -err(0) * x(1) + l2 * w(0, 2) + l1 * signum(w(0, 2))

            val dj10 = -err(1)
            val dj11 = -err(1) * x(0) + l2 * w(1, 1) + l1 * signum(w(1, 1))
            val dj12 = -err(1) * x(1) + l2 * w(1, 2) + l1 * signum(w(1, 2))

            dj(0, 0) shouldBe dj00 +- 1e-6
            dj(0, 1) shouldBe dj01 +- 1e-6
            dj(0, 2) shouldBe dj02 +- 1e-6
            dj(1, 0) shouldBe dj10 +- 1e-6
            dj(1, 1) shouldBe dj11 +- 1e-6
            dj(1, 2) shouldBe dj12 +- 1e-6
          }
      }
  }

  property("gradSigmoid") {
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)
    val xGen = MazeGen.vector(m - 1, 10.0)

    val errGen = MazeGen.vector(n, 1e-3)
    val hGen = MazeGen.vector(n, 1)
    val l1Gen = Gen.choose(0.0, 1e3)
    val l2Gen = Gen.choose(0.0, 1e3)

    forAll(
      (l1Gen, "l1"),
      (l2Gen, "l2"),
      (errGen, "err"),
      (hGen, "h"),
      (xGen, "x"),
      (wGen, "w")) {
        (l1, l2, err, h, x, w) =>
          {
            val func = CostFunction.elasticNet(l1, l2)
            val y = h + err
            val dj = func.grad(err, Sigmoid.grad(h, x), x, w)

            val dj00 = -err(0) * h(0) * (1 - h(0))
            val dj01 = -err(0) * h(0) * (1 - h(0)) * x(0) + l1 * signum(w(0, 1)) + l2 * w(0, 1)
            val dj02 = -err(0) * h(0) * (1 - h(0)) * x(1) + l1 * signum(w(0, 2)) + l2 * w(0, 2)

            val dj10 = -err(1) * h(1) * (1 - h(1))
            val dj11 = -err(1) * h(1) * (1 - h(1)) * x(0) + l1 * signum(w(1, 1)) + l2 * w(1, 1)
            val dj12 = -err(1) * h(1) * (1 - h(1)) * x(1) + l1 * signum(w(1, 2)) + l2 * w(1, 2)

            dj(0, 0) shouldBe dj00 +- 1e-6
            dj(0, 1) shouldBe dj01 +- 1e-6
            dj(0, 2) shouldBe dj02 +- 1e-6

            dj(1, 0) shouldBe dj10 +- 1e-6
            dj(1, 1) shouldBe dj11 +- 1e-6
            dj(1, 2) shouldBe dj12 +- 1e-6
          }
      }
  }

  property("gradTanh") {
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)
    val xGen = MazeGen.vector(m - 1, 10.0)

    val errGen = MazeGen.vector(n, 1e-3)
    val hGen = MazeGen.vector(n, 1)
    val l1Gen = Gen.choose(0.0, 1e3)
    val l2Gen = Gen.choose(0.0, 1e3)

    forAll(
      (l1Gen, "l1"),
      (l2Gen, "l2"),
      (errGen, "err"),
      (hGen, "h"),
      (xGen, "x"),
      (wGen, "w")) {
        (l1, l2, err, h, x, w) =>
          {
            val func = CostFunction.elasticNet(l1, l2)
            val y = h + err
            val dj = func.grad(y - h, Tanh.grad(h, x), x, w)

            val dj00 = -err(0) * (1 + h(0)) * (1 - h(0))
            val dj01 = -err(0) * (1 + h(0)) * (1 - h(0)) * x(0) + l1 * signum(w(0, 1)) + l2 * w(0, 1)
            val dj02 = -err(0) * (1 + h(0)) * (1 - h(0)) * x(1) + l1 * signum(w(0, 2)) + l2 * w(0, 2)

            val dj10 = -err(1) * (1 + h(1)) * (1 - h(1))
            val dj11 = -err(1) * (1 + h(1)) * (1 - h(1)) * x(0) + l1 * signum(w(1, 1)) + l2 * w(1, 1)
            val dj12 = -err(1) * (1 + h(1)) * (1 - h(1)) * x(1) + l1 * signum(w(1, 2)) + l2 * w(1, 2)

            dj(0, 0) shouldBe dj00 +- 1e-6
            dj(0, 1) shouldBe dj01 +- 1e-6
            dj(0, 2) shouldBe dj02 +- 1e-6

            dj(1, 0) shouldBe dj10 +- 1e-6
            dj(1, 1) shouldBe dj11 +- 1e-6
            dj(1, 2) shouldBe dj12 +- 1e-6
          }
      }
  }
}
