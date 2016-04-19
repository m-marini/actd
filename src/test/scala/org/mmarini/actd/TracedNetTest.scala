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

import breeze.stats.distributions.Rand

class TracedNetTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  val InputRange = 1e2
  val OutputRange = 1e2

  val inGen = MazeGen.vector(1, InputRange)
  val nlrOutGen = MazeGen.vector(1, OutputRange)
  val layers = Seq(1, 1, 1)

  val tdParmsGen = for {
    alpha <- Gen.choose(0.0, 1.0)
    gamma <- Gen.choose(0.0, 1.0)
    lambda <- Gen.choose(0.0, 1.0)
    eta <- Gen.choose(0.0, 1e-3)
  } yield TDParms(
    alpha = alpha,
    beta = 0.0,
    gamma = gamma,
    epsilon = 0.0,
    lambda = lambda,
    eta = eta,
    random = Rand)

  property("Traces and weights changed") {

    Given("td parameters")
    And("an input vector")
    And("an the expected vector")

    When("learns")

    Then("the result net should have traced and weights changed")

    forAll(
      (tdParmsGen, "parms"),
      (inGen, "in"),
      (nlrOutGen, "out")) {
        (parms, in, out) =>
          {
            val net = TDNeuralNet(layers, parms)
            val net1 = net.learn(out, in)

            net1.trace should have size (net.trace.size)
            net1.weights should have size (net.weights.size)

            net1.trace.matrices should not contain theSameElementsAs(net.trace.matrices)
            net1.weights.matrices should not contain theSameElementsAs(net.weights.matrices)
          }
      }
  }
}
