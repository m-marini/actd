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

/**
 * @author us00852
 */
class ActivationFunctionTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  private val InputRange = 100.0
  private val Dimensions = 10

  property("grad") {
    val funcGen = Gen.oneOf(Ident, Sigmoid, Tanh)
    val xGen = MazeGen.vector(Dimensions, InputRange)
    val dxGen = MazeGen.vector(Dimensions, Gen.choose(1e-4, 2e-4))

    forAll(
      (funcGen, "func"),
      (xGen, "x"),
      (dxGen, "dx")) {
        (func, x, dx) =>
          {
            val y = func(x)
            val y1 = func(x + dx)
            val dydx = (y1 - y) :/ dx
            val grad = func.grad(y, x)

            TestFuncs.vectorLike(grad, dydx, 1e-4)
          }
      }
  }
}
