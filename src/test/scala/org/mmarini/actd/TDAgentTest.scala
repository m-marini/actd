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

import scala.math.pow
import org.scalacheck.Gen
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.PropSpec
import org.scalatest.prop.PropertyChecks
import breeze.linalg.DenseVector
import breeze.stats.distributions.Rand
import breeze.stats.distributions.RandBasis
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.random.MersenneTwister

class TDAgentTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  val Epsilon = 1e-5
  val HiddenLayer = 4
  val MinIteration = 100
  val MaxIteration = 100
  val InputSize = 4
  val MinHidden = 4
  val MaxHidden = 10
  val layers = Seq(InputSize, HiddenLayer, 1)
  val Seed = 123L

  private val s0: Status = new Status() {
    val toDenseVector = DenseVector(0.0, 0.0)
    def apply(a: Action) = Feedback(this, a, -1.0, s1)
  }

  private val s1: Status = new Status() {
    val toDenseVector = DenseVector(1.0, 0.0)
    def apply(a: Action) = Feedback(this, a, 1.0, s2)
  }

  private val s2: Status = new Status() {
    val toDenseVector = DenseVector(0.0, 1.0)
    override val finalStatus = true
    def apply(a: Action) = Feedback(this, a, 0.0, s0)
  }

  val agentGen = for {
    alpha <- Gen.const(0.0)
    beta <- Gen.const(0.1)
    gamma <- Gen.choose(0.9, 0.9)
    lambda <- Gen.choose(0.0, 0.0)
    eta <- Gen.const(1e-1)
    hidden <- Gen.choose(MinHidden, MaxHidden)
  } yield TDAgent(
    TDParms(
      alpha = alpha,
      beta = beta,
      gamma = gamma,
      epsilon = 0.0,
      lambda = lambda,
      eta = eta,
      random = new RandBasis(new MersenneTwister(Seed))),
    1.0,
    2, 2)

  property("Traced critic state function") {

    Given("a deterministic MDP process with 3 state and 2 action")
    And("a TD agent")
    And("a sequence of state action by a policy")

    When("learns in 3 steps episode iteratively")

    Then("the critic weights should approach w2 = S2 = 0")
    And("the critic weights should approach w1 = S1 = 1")
    And("the critic weights should approach w0 = S0 = -1 + 0.9 = -0.1")

    forAll(
      (agentGen, "agent"),
      (Gen.choose(MinIteration, MaxIteration), "n")) {
        (agent, n) =>
          whenever(agent.parms.gamma < 1.0) {
            val episode = Seq(
              Feedback(s0, 0, -1.0, s1),
              Feedback(s1, 1, 1.0, s2),
              Feedback(s2, 0, 0.0, s0))

            val iter = (1 to n)

            def learnEpisode(ag: TDAgent): TDAgent = {

              val ag1 = episode.foldLeft(ag) {
                case (ag, feedback) =>
                  ag.train(feedback)._1
              }
              ag1
            }

            def learn1(ag: TDAgent): TDAgent = {
              iter.foldLeft(ag)((ag0, i) => {
                learnEpisode(ag0)
              })
            }

            val agent2 = learn1(agent)

            val gamma = agent.parms.gamma

            val exp2 = 0.0
            val exp1 = 1.0
            val exp0 = -1.0 + exp1 * gamma
            val expMap = Map((s0 -> exp0), (s1 -> exp1), (s2 -> exp2))

            def error(agent: TDAgent): Double =
              expMap.map { case (s, e) => e - agent.critic(s.toDenseVector).output(0) }.map(x => x * x).sum

            val w00 = agent.critic.weights.matrices(0)(0, 0)
            val w01 = agent.critic.weights.matrices(0)(0, 1)
            val w02 = agent.critic.weights.matrices(0)(0, 2)

            val w10 = agent2.critic.weights.matrices(0)(0, 0)
            val w11 = agent2.critic.weights.matrices(0)(0, 1)
            val w12 = agent2.critic.weights.matrices(0)(0, 2)

            info(s"agent0 ${agent.critic.weights}")
            info(s"agent2 ${agent2.critic.weights}")

            pow(w10 - exp0, 2) should be <= (pow(w00 - exp0, 2))
            pow(w11 - exp1, 2) should be <= (pow(w01 - exp1, 2))
            pow(w12 - exp2, 2) should be <= (pow(w02 - exp2, 2))

            //            w10 shouldBe (exp0) +- 0.1
            //            w11 shouldBe (exp1) +- 0.1
            //            w12 shouldBe (exp2) +- 0.1

            //def error(agent: TDAgent): Double = {
            //              pow( - exp0, 2) +
            //                pow(agent.critic.weight= exps.weights(0)(0, 1) - exp1, 2) +
            //                pow(agent.critic.weights.weights(0)(0, 2) - exp2, 2)
            //            }

            //            error(agent2) should be <= error(agent)
          }
      }
  }

  property("Critic state function") {

    Given("a deterministic MDP process with 2 state and 2 action")
    And("a TD agent")

    When("learns in a step")

    Then("the critic should better evaluate the state")

    forAll(
      (agentGen, "agent"),
      (Gen.choose(-1e3, 1e3), "reward")) {
        (agent, reward) =>
          {
            val (agent1, _) = agent.train(Feedback(s0, 1, reward, s1))

            val e0 = pow(reward - agent.critic(s0.toDenseVector).output(0), 2)
            val e1 = pow(reward - agent1.critic(s0.toDenseVector).output(0), 2)
            e1 should be <= e0
          }
      }
  }

  property("Actor action function") {

    Given("a deterministic MDP process with 2 state and 2 action")
    And("a TD agent")

    When("learns in a step")

    Then("the actor should better evaluate the action preference")

    forAll(
      (agentGen, "agent"),
      (Gen.oneOf(-10.0, 10.0), "reward"),
      (Gen.choose(0, 1), "action")) {
        (agent, reward, action) =>
          {
            val (agent1, _) = agent.train(Feedback(s0, action, reward, s1))

            val p00 = agent.actor(s0.toDenseVector).output(action)
            val p01 = agent1.actor(s0.toDenseVector).output(action)
            if (reward > 0) {
              p01 should be > p00
            } else {
              p01 should be < p00
            }
          }
      }
  }
}
