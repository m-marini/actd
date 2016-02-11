/**
 *
 */
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

  val epsilonGen = Gen.choose(10e-3, 100e-3)

  property("Traces and weights changed") {

    Given("td parameters")
    And("an epsilon randomizer parameter")
    And("an input vector")
    And("an the expected vector")

    When("learns")

    Then("the result net should have traced and weights changed")

    forAll(
      (tdParmsGen, "parms"),
      (epsilonGen, "epsilon"),
      (inGen, "in"),
      (nlrOutGen, "out")) {
        (parms, epsilon, in, out) =>
          {
            val net = TDNeuralNet(layers, parms, epsilon)
            val net1 = net.learn(out, in)

            net1.trace should have size (net.trace.size)
            net1.weights should have size (net.weights.size)

            net1.trace.matrices should not contain theSameElementsAs(net.trace.matrices)
            net1.weights.matrices should not contain theSameElementsAs(net.weights.matrices)
          }
      }
  }
}
