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

import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen
import org.scalatest.PropSpec
import breeze.linalg._
import scala.math.exp
import org.scalatest.GivenWhenThen
import breeze.linalg.operators.DenseVector_GenericOps
import breeze.linalg.operators.DenseVector_HashVector_Ops
import breeze.stats.distributions.Rand

class TDNeuralNetTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {
  val Epsilon = 1e-5
  val HiddenLayer = 4
  val MinIteration = 20
  val MaxIteration = 1000
  val InputSize = 4
  val layers = Seq(InputSize, HiddenLayer, 1)

  val parmsGen = for {
    alpha <- Gen.choose(0.0, 0.9)
    gamma <- Gen.choose(0.5, 1.0)
    lambda <- Gen.choose(0.0, 1.0)
    eta <- Gen.choose(1e-6, 1e-3)
  } yield TDParms(
    alpha = alpha,
    beta = 0.0,
    gamma = gamma,
    epsilon = 0.0,
    lambda = lambda,
    eta = eta,
    random = Rand)

  property("Learning rate") {

    Given("a deterministic MDP process with 4 state")
    And("the state values of MDP")
    And("a sequence of episodes for MDP")
    And("a traced net with TD parameters")
    And("the error over the sequence")

    When("learns by iterating")
    And("computing error")

    Then("the error at the end of learning step should be less then initial one")

    forAll(
      (parmsGen, "parms"),
      (Gen.choose(MinIteration, MaxIteration), "n")) {
        (parms, n) =>
          {
            val net = TDNeuralNet(layers, parms, Epsilon)

            val S0 = DenseVector(1.0, 0.0, 0.0, 0.0)
            val S1 = DenseVector(0.0, 1.0, 0.0, 0.0)
            val S2 = DenseVector(0.0, 0.0, 1.0, 0.0)
            val S3 = DenseVector(0.0, 0.0, 0.0, 1.0)

            val R01 = DenseVector(10.0)
            val R13 = DenseVector(2.0)
            val R23 = DenseVector(5.0)
            val R33 = DenseVector(0.0)

            /*
     * S0 -> (S1, R01)
     * S1 -> (S3, R13)
     * S2 -> (S3, R23)
     * S3 -> (S3, R33)
     */
            val r = Map(
              S0 -> R01,
              S1 -> R13,
              S2 -> R23,
              S3 -> R33)

            val g = parms.gamma
            val v = Map(
              S0 -> (R33 + R13 * g + R01 * (g * g)),
              S1 -> (R33 + R13 * g),
              S2 -> (R33 + R23 * g),
              S3 -> R33)

            val samples = for {
              ep <- Seq(
                Seq(S0, S1, S3),
                Seq(S2, S3))
            } yield ep.map(s => (s, r(s)))

            def learnLoop(net: TDNeuralNet): TDNeuralNet = {
              (0 to n).foldLeft(net)((net, _) => learnSamples(net))
            }

            def learnSamples(net: TDNeuralNet): TDNeuralNet =
              samples.foldLeft(net)(learnEpisode)

            def learnEpisode(net: TDNeuralNet, episode: Seq[(DenseVector[Double], DenseVector[Double])]): TDNeuralNet =
              episode.foldLeft(net.clearTraces) {
                case (net, (in, out)) => net.learn(in, out)
              }

            def computeError(net: TDNeuralNet): Double = {
              val errs = for {
                (in, out) <- r
              } yield {
                val x = out(0) - net(in).output(0)
                x * x
              }
              errs.sum
            }

            val net1 = learnLoop(net)

            val err0 = computeError(net)

            val err1 = computeError(net1)

            err1 should be < (err0)
            info(s"Initial error $err0, after $n iterations $err1")
          }
      }
  }
}
