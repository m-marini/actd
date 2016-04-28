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

import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalatest.PropSpec
import org.scalacheck.Gen
import breeze.stats.distributions.Rand
import breeze.linalg.DenseVector
import scala.math.tanh
import breeze.linalg.DenseMatrix

/**
 * @author us00852
 */
class TDNeuralNet1Test extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  val InputRange = 1e2
  val OutputRange = 1e2

  val inGen = MazeGen.vector(2, InputRange)
  val nlrOutGen = MazeGen.vector(2, OutputRange)
  val hiddenOutGen = MazeGen.vector(2, 1.0)

  val tdParmsGen = for {
    alpha <- Gen.choose(0.0, 1.0)
    gamma <- Gen.choose(0.0, 1.0)
    lambda <- Gen.choose(0.0, 1.0)
    eta <- Gen.choose(0.0, 1e-5)
  } yield TDParms(
    alpha = alpha,
    beta = 0.0,
    gamma = gamma,
    epsilon = 0.0,
    lambda = lambda,
    eta = eta,
    random = Rand)

  property("net output") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in")) {
        (tdParms, in) =>
          {
            val net = TDNeuralNet1(tdParms)(Seq(2, 3, 2))
            val status = net(in)
            val out = status.output
            val w0 = net.layers(0).weights
            val w1 = net.layers(1).weights
            val z00 = w0(0, 0) + w0(0, 1) * in(0) + w0(0, 2) * in(1)
            val z01 = w0(1, 0) + w0(1, 1) * in(0) + w0(1, 2) * in(1)
            val z02 = w0(2, 0) + w0(2, 1) * in(0) + w0(2, 2) * in(1)
            val h00 = tanh(z00)
            val h01 = tanh(z01)
            val h02 = tanh(z02)

            val h10 = w1(0, 0) + w1(0, 1) * h00 + w1(0, 2) * h01 + w1(0, 3) * h02
            val h11 = w1(1, 0) + w1(1, 1) * h00 + w1(1, 2) * h01 + w1(1, 3) * h02

            out(0) shouldBe h10 +- 1e-6
            out(1) shouldBe h11 +- 1e-6
          }
      }
  }
}
