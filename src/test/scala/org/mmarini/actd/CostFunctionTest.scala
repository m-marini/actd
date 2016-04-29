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

/**
 * @author us00852
 */
class CostFunctionTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  property("cost") {
    val errGen = Gen.choose(0.0, 1e-3)
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)

    val hGen = MazeGen.vector(n, 1)
    val alphaGen = Gen.choose(0.0, 1.0)

    forAll(
      (alphaGen, "alpha"),
      (errGen, "err"),
      (hGen, "h"),
      (wGen, "w")) {
        (alpha, err, h, w) =>
          {
            val func = CostFunction(alpha)
            val y = h + err
            val j = func(y - h, w)
            j shouldBe (((1 - alpha) * err * err * n + alpha *
              (w(0, 1) * w(0, 1) +
                w(0, 2) * w(0, 2) +
                w(1, 1) * w(1, 1) +
                w(1, 2) * w(1, 2))) / 2) +- 1e-6
          }
      }
  }

  property("gradIdent") {
    val errGen = Gen.choose(0.0, 1e-3)
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)
    val xGen = MazeGen.vector(m - 1, 10.0)

    val hGen = MazeGen.vector(n, 1)
    val alphaGen = Gen.choose(0.0, 1.0)

    forAll(
      (alphaGen, "alpha"),
      (errGen, "err"),
      (hGen, "h"),
      (xGen, "x"),
      (wGen, "w")) {
        (alpha, err, h, x, w) =>
          {
            val func = CostFunction(alpha)
            val y = h + err
            val dj = func.grad(y - h, Ident.grad(h, x), x, w)

            val expected = DenseMatrix(
              (-(1 - alpha) * err, -(1 - alpha) * err * x(0) + alpha * w(0, 1), -(1 - alpha) * err * x(1) + alpha * w(0, 2)),
              (-(1 - alpha) * err, -(1 - alpha) * err * x(0) + alpha * w(1, 1), -(1 - alpha) * err * x(1) + alpha * w(1, 2)))
            TestFuncs.matrixLike(dj, expected, 1e-3)
          }
      }
  }

  property("gradSigmoid") {
    val errGen = Gen.choose(0.0, 1e-3)
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)
    val xGen = MazeGen.vector(m - 1, 10.0)

    val hGen = MazeGen.vector(n, 1)
    val alphaGen = Gen.choose(0.0, 1.0)

    forAll(
      (alphaGen, "alpha"),
      (errGen, "err"),
      (hGen, "h"),
      (xGen, "x"),
      (wGen, "w")) {
        (alpha, err, h, x, w) =>
          {
            val func = CostFunction(alpha)
            val y = h + err
            val dj = func.grad(y - h, Sigmoid.grad(h, x), x, w)

            val expected = DenseMatrix(
              (-(1 - alpha) * err * h(0) * (1 - h(0)),
                -(1 - alpha) * err * x(0) * h(0) * (1 - h(0)) + alpha * w(0, 1),
                -(1 - alpha) * err * x(1) * h(0) * (1 - h(0)) + alpha * w(0, 2)),
              (-(1 - alpha) * err * h(1) * (1 - h(1)),
                -(1 - alpha) * err * x(0) * h(1) * (1 - h(1)) + alpha * w(1, 1),
                -(1 - alpha) * err * x(1) * h(1) * (1 - h(1)) + alpha * w(1, 2)))
            TestFuncs.matrixLike(dj, expected, 1e-3)
          }
      }
  }

  property("gradTanh") {
    val errGen = Gen.choose(0.0, 1e-3)
    val n = 2
    val m = 3
    val wGen = MazeGen.matrix(n, m, 10.0)
    val xGen = MazeGen.vector(m - 1, 10.0)

    val hGen = MazeGen.vector(n, 1)
    val alphaGen = Gen.choose(0.0, 1.0)

    forAll(
      (alphaGen, "alpha"),
      (errGen, "err"),
      (hGen, "h"),
      (xGen, "x"),
      (wGen, "w")) {
        (alpha, err, h, x, w) =>
          {
            val func = CostFunction(alpha)
            val y = h + err
            val dj = func.grad(y - h, Tanh.grad(h, x), x, w)

            val expected = DenseMatrix(
              (-(1 - alpha) * err * (1 + h(0)) * (1 - h(0)),
                -(1 - alpha) * err * x(0) * (1 + h(0)) * (1 - h(0)) + alpha * w(0, 1),
                -(1 - alpha) * err * x(1) * (1 + h(0)) * (1 - h(0)) + alpha * w(0, 2)),
              (-(1 - alpha) * err * (1 + h(1)) * (1 - h(1)),
                -(1 - alpha) * err * x(0) * (1 + h(1)) * (1 - h(1)) + alpha * w(1, 1),
                -(1 - alpha) * err * x(1) * (1 + h(1)) * (1 - h(1)) + alpha * w(1, 2)))
            TestFuncs.matrixLike(dj, expected, 1e-3)
          }
      }
  }
}
