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
import scala.math.signum
import breeze.linalg.DenseMatrix
import breeze.linalg.sum
import breeze.stats.distributions.RandBasis
import org.apache.commons.math3.random.MersenneTwister

/**
 * @author us00852
 */
class TDNeuralNet1Test extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  val Seed = 1234L

  val InputRange = 1e2
  val OutputRange = 1e2

  val inGen = MazeGen.vector(2, InputRange)
  val outGen = MazeGen.vector(2, OutputRange)

  //  val tdParmsGen = for {
  //    alpha <- Gen.const(0.5)
  //    gamma <- Gen.const(0.9)
  //    lambda <- Gen.const(0.9)
  //    eta <- Gen.const(1e-5)
  //  } yield TDParms(
  //    alpha = alpha,
  //    beta = 0.0,
  //    gamma = gamma,
  //    epsilon = 0.0,
  //    lambda = lambda,
  //    eta = eta,
  //    random = Rand)

  val tdParmsGen = for {
    alpha <- Gen.choose(0.0, 1.0)
    gamma <- Gen.choose(0.0, 1.0)
    lambda <- Gen.choose(0.0, 1.0)
    eta <- Gen.choose(0.0, 100e-3)
  } yield TDParms(
    alpha = alpha,
    beta = 0.0,
    gamma = gamma,
    epsilon = 0.0,
    lambda = lambda,
    eta = eta,
    random = new RandBasis(new MersenneTwister(Seed)))

  property("net output") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in")) {
        (tdParms, in) =>
          {
            val net = TDNeuralNet(tdParms)(Seq(2, 3, 2))
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

  property("net cost") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "x"),
      (outGen, "y")) {
        (tdParms, x, y) =>
          {
            val net = TDNeuralNet(tdParms)(Seq(2, 3, 2))
            val status = net(x)
            val h = status.output
            val delta = y - h
            val cost = status.cost(delta)

            val w0 = net.layers(0).weights
            val w1 = net.layers(1).weights

            val wr0 = w0(::, 1 to -1)
            val wr1 = w1(::, 1 to -1)

            val expCost = (1 - tdParms.alpha) * sum(delta :* delta) / 2 +
              tdParms.alpha * (sum(wr0 :* wr0) + sum(wr1 :* wr1)) / 2

            cost shouldBe expCost +- 1e-6
          }
      }
  }

  property("net train") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "x"),
      (outGen, "y")) {
        (tdParms, x, y) =>
          {
            val net = TDNeuralNet(tdParms)(Seq(2, 3, 2))
            val status = net(x)
            val h = status.output
            val delta = y - h
            val net1 = status.train(delta)

            val w0 = net.layers(0).weights
            val w1 = net.layers(1).weights
            val z00 = w0(0, 0) + w0(0, 1) * x(0) + w0(0, 2) * x(1)
            val z01 = w0(1, 0) + w0(1, 1) * x(0) + w0(1, 2) * x(1)
            val z02 = w0(2, 0) + w0(2, 1) * x(0) + w0(2, 2) * x(1)
            val h00 = tanh(z00)
            val h01 = tanh(z01)
            val h02 = tanh(z02)

            val h10 = w1(0, 0) + w1(0, 1) * h00 + w1(0, 2) * h01 + w1(0, 3) * h02
            val h11 = w1(1, 0) + w1(1, 1) * h00 + w1(1, 2) * h01 + w1(1, 3) * h02

            val e0 = net1.layers(0).traces
            val e1 = net1.layers(1).traces
            val alpha = tdParms.alpha

            val e100 = (1 - alpha) * delta(0)
            val e101 = (1 - alpha) * delta(0) * h00 - alpha * w1(0, 1)
            val e102 = (1 - alpha) * delta(0) * h01 - alpha * w1(0, 2)
            val e103 = (1 - alpha) * delta(0) * h02 - alpha * w1(0, 3)

            val eta = tdParms.eta

            val w100 = w1(0, 0) + eta * e100
            val w101 = w1(0, 1) + eta * e101
            val w102 = w1(0, 2) + eta * e102
            val w103 = w1(0, 3) + eta * e103

            val e110 = (1 - alpha) * delta(1)
            val e111 = (1 - alpha) * delta(1) * h00 - alpha * w1(1, 1)
            val e112 = (1 - alpha) * delta(1) * h01 - alpha * w1(1, 2)
            val e113 = (1 - alpha) * delta(1) * h02 - alpha * w1(1, 3)

            val w110 = w1(1, 0) + eta * e110
            val w111 = w1(1, 1) + eta * e111
            val w112 = w1(1, 2) + eta * e112
            val w113 = w1(1, 3) + eta * e113

            val delta0 = delta(0) * w1(0, 1) + delta(1) * w1(1, 1)
            val delta1 = delta(0) * w1(0, 2) + delta(1) * w1(1, 2)
            val delta2 = delta(0) * w1(0, 3) + delta(1) * w1(1, 3)

            val dj000 = -(1 - alpha) * delta0 * (1 + h00) * (1 - h00)
            val dj001 = -(1 - alpha) * delta0 * x(0) * (1 + h00) * (1 - h00) + alpha * w0(0, 1)
            val dj002 = -(1 - alpha) * delta0 * x(1) * (1 + h00) * (1 - h00) + alpha * w0(0, 2)

            val e000 = -signum(dj000)
            val e001 = -signum(dj001)
            val e002 = -signum(dj002)

            val w000 = w0(0, 0) + eta * e000
            val w001 = w0(0, 1) + eta * e001
            val w002 = w0(0, 2) + eta * e002

            val dj010 = -(1 - alpha) * delta1 * (1 + h01) * (1 - h01)
            val dj011 = -(1 - alpha) * delta1 * x(0) * (1 + h01) * (1 - h01) + alpha * w0(1, 1)
            val dj012 = -(1 - alpha) * delta1 * x(1) * (1 + h01) * (1 - h01) + alpha * w0(1, 2)

            val e010 = -signum(dj010)
            val e011 = -signum(dj011)
            val e012 = -signum(dj012)

            val w010 = w0(1, 0) + eta * e010
            val w011 = w0(1, 1) + eta * e011
            val w012 = w0(1, 2) + eta * e012

            val dj020 = -(1 - alpha) * delta2 * (1 + h02) * (1 - h02)
            val dj021 = -(1 - alpha) * delta2 * x(0) * (1 + h02) * (1 - h02) + alpha * w0(2, 1)
            val dj022 = -(1 - alpha) * delta2 * x(1) * (1 + h02) * (1 - h02) + alpha * w0(2, 2)

            val e020 = -signum(dj020)
            val e021 = -signum(dj021)
            val e022 = -signum(dj022)

            val w020 = w0(2, 0) + eta * e020
            val w021 = w0(2, 1) + eta * e021
            val w022 = w0(2, 2) + eta * e022

            //************************************************

            e1(0, 0) shouldBe e100 +- 1e-6
            e1(0, 1) shouldBe e101 +- 1e-6
            e1(0, 2) shouldBe e102 +- 1e-6
            e1(0, 3) shouldBe e103 +- 1e-6

            e1(1, 0) shouldBe e110 +- 1e-6
            e1(1, 1) shouldBe e111 +- 1e-6
            e1(1, 2) shouldBe e112 +- 1e-6
            e1(1, 3) shouldBe e113 +- 1e-6

            e0(0, 0) shouldBe e000 +- 1e-6
            e0(0, 1) shouldBe e001 +- 1e-6
            e0(0, 2) shouldBe e002 +- 1e-6

            e0(1, 0) shouldBe e010 +- 1e-6
            e0(1, 1) shouldBe e011 +- 1e-6
            e0(1, 2) shouldBe e012 +- 1e-6

            e0(2, 0) shouldBe e020 +- 1e-6
            e0(2, 1) shouldBe e021 +- 1e-6
            e0(2, 2) shouldBe e022 +- 1e-6

            net1.layers(1).weights(0, 0) shouldBe w100 +- 1e-6
            net1.layers(1).weights(0, 1) shouldBe w101 +- 1e-6
            net1.layers(1).weights(0, 2) shouldBe w102 +- 1e-6
            net1.layers(1).weights(0, 3) shouldBe w103 +- 1e-6

            net1.layers(1).weights(1, 0) shouldBe w110 +- 1e-6
            net1.layers(1).weights(1, 1) shouldBe w111 +- 1e-6
            net1.layers(1).weights(1, 2) shouldBe w112 +- 1e-6
            net1.layers(1).weights(1, 3) shouldBe w113 +- 1e-6

            net1.layers(0).weights(0, 0) shouldBe w000 +- 1e-6
            net1.layers(0).weights(0, 1) shouldBe w001 +- 1e-6
            net1.layers(0).weights(0, 2) shouldBe w002 +- 1e-6

            net1.layers(0).weights(1, 0) shouldBe w010 +- 1e-6
            net1.layers(0).weights(1, 1) shouldBe w011 +- 1e-6
            net1.layers(0).weights(1, 2) shouldBe w012 +- 1e-6

            net1.layers(0).weights(2, 0) shouldBe w020 +- 1e-6
            net1.layers(0).weights(2, 1) shouldBe w021 +- 1e-6
            net1.layers(0).weights(2, 2) shouldBe w022 +- 1e-6
          }
      }
  }

  property("net train with traces") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "x"),
      (outGen, "y")) {
        (tdParms, x, y) =>
          {
            val s0 = TDNeuralNet(tdParms)(Seq(2, 3, 2))(x)
            val net = s0.train(y - s0.output)
            val status = net(x)
            val h = status.output
            val delta = y - h
            val net1 = status.train(delta)

            val w0 = net.layers(0).weights
            val w1 = net.layers(1).weights
            val z00 = w0(0, 0) + w0(0, 1) * x(0) + w0(0, 2) * x(1)
            val z01 = w0(1, 0) + w0(1, 1) * x(0) + w0(1, 2) * x(1)
            val z02 = w0(2, 0) + w0(2, 1) * x(0) + w0(2, 2) * x(1)
            val h00 = tanh(z00)
            val h01 = tanh(z01)
            val h02 = tanh(z02)

            val h10 = w1(0, 0) + w1(0, 1) * h00 + w1(0, 2) * h01 + w1(0, 3) * h02
            val h11 = w1(1, 0) + w1(1, 1) * h00 + w1(1, 2) * h01 + w1(1, 3) * h02

            val e0 = net1.layers(0).traces
            val e1 = net1.layers(1).traces

            val alpha = tdParms.alpha
            val decay = tdParms.gamma * tdParms.lambda

            val e100 = decay * net.layers(1).traces(0, 0) + (1 - alpha) * delta(0)
            val e101 = decay * net.layers(1).traces(0, 1) + (1 - alpha) * delta(0) * h00 - alpha * w1(0, 1)
            val e102 = decay * net.layers(1).traces(0, 2) + (1 - alpha) * delta(0) * h01 - alpha * w1(0, 2)
            val e103 = decay * net.layers(1).traces(0, 3) + (1 - alpha) * delta(0) * h02 - alpha * w1(0, 3)

            val eta = tdParms.eta

            val w100 = w1(0, 0) + eta * e100
            val w101 = w1(0, 1) + eta * e101
            val w102 = w1(0, 2) + eta * e102
            val w103 = w1(0, 3) + eta * e103

            val e110 = decay * net.layers(1).traces(1, 0) + (1 - alpha) * delta(1)
            val e111 = decay * net.layers(1).traces(1, 1) + (1 - alpha) * delta(1) * h00 - alpha * w1(1, 1)
            val e112 = decay * net.layers(1).traces(1, 2) + (1 - alpha) * delta(1) * h01 - alpha * w1(1, 2)
            val e113 = decay * net.layers(1).traces(1, 3) + (1 - alpha) * delta(1) * h02 - alpha * w1(1, 3)

            val w110 = w1(1, 0) + eta * e110
            val w111 = w1(1, 1) + eta * e111
            val w112 = w1(1, 2) + eta * e112
            val w113 = w1(1, 3) + eta * e113

            val delta0 = delta(0) * w1(0, 1) + delta(1) * w1(1, 1)
            val delta1 = delta(0) * w1(0, 2) + delta(1) * w1(1, 2)
            val delta2 = delta(0) * w1(0, 3) + delta(1) * w1(1, 3)

            val dj000 = -(1 - alpha) * delta0 * (1 + h00) * (1 - h00)
            val dj001 = -(1 - alpha) * delta0 * x(0) * (1 + h00) * (1 - h00) + alpha * w0(0, 1)
            val dj002 = -(1 - alpha) * delta0 * x(1) * (1 + h00) * (1 - h00) + alpha * w0(0, 2)

            val e000 = decay * net.layers(0).traces(0, 0) - signum(dj000)
            val e001 = decay * net.layers(0).traces(0, 1) - signum(dj001)
            val e002 = decay * net.layers(0).traces(0, 2) - signum(dj002)

            val w000 = w0(0, 0) + eta * e000
            val w001 = w0(0, 1) + eta * e001
            val w002 = w0(0, 2) + eta * e002

            val dj010 = -(1 - alpha) * delta1 * (1 + h01) * (1 - h01)
            val dj011 = -(1 - alpha) * delta1 * x(0) * (1 + h01) * (1 - h01) + alpha * w0(1, 1)
            val dj012 = -(1 - alpha) * delta1 * x(1) * (1 + h01) * (1 - h01) + alpha * w0(1, 2)

            val e010 = decay * net.layers(0).traces(1, 0) - signum(dj010)
            val e011 = decay * net.layers(0).traces(1, 1) - signum(dj011)
            val e012 = decay * net.layers(0).traces(1, 2) - signum(dj012)

            val w010 = w0(1, 0) + eta * e010
            val w011 = w0(1, 1) + eta * e011
            val w012 = w0(1, 2) + eta * e012

            val dj020 = -(1 - alpha) * delta2 * (1 + h02) * (1 - h02)
            val dj021 = -(1 - alpha) * delta2 * x(0) * (1 + h02) * (1 - h02) + alpha * w0(2, 1)
            val dj022 = -(1 - alpha) * delta2 * x(1) * (1 + h02) * (1 - h02) + alpha * w0(2, 2)

            val e020 = decay * net.layers(0).traces(2, 0) - signum(dj020)
            val e021 = decay * net.layers(0).traces(2, 1) - signum(dj021)
            val e022 = decay * net.layers(0).traces(2, 2) - signum(dj022)

            val w020 = w0(2, 0) + eta * e020
            val w021 = w0(2, 1) + eta * e021
            val w022 = w0(2, 2) + eta * e022

            //************************************************

            e1(0, 0) shouldBe e100 +- 1e-6
            e1(0, 1) shouldBe e101 +- 1e-6
            e1(0, 2) shouldBe e102 +- 1e-6
            e1(0, 3) shouldBe e103 +- 1e-6

            e1(1, 0) shouldBe e110 +- 1e-6
            e1(1, 1) shouldBe e111 +- 1e-6
            e1(1, 2) shouldBe e112 +- 1e-6
            e1(1, 3) shouldBe e113 +- 1e-6

            e0(0, 0) shouldBe e000 +- 1e-6
            e0(0, 1) shouldBe e001 +- 1e-6
            e0(0, 2) shouldBe e002 +- 1e-6

            e0(1, 0) shouldBe e010 +- 1e-6
            e0(1, 1) shouldBe e011 +- 1e-6
            e0(1, 2) shouldBe e012 +- 1e-6

            e0(2, 0) shouldBe e020 +- 1e-6
            e0(2, 1) shouldBe e021 +- 1e-6
            e0(2, 2) shouldBe e022 +- 1e-6

            net1.layers(1).weights(0, 0) shouldBe w100 +- 1e-6
            net1.layers(1).weights(0, 1) shouldBe w101 +- 1e-6
            net1.layers(1).weights(0, 2) shouldBe w102 +- 1e-6
            net1.layers(1).weights(0, 3) shouldBe w103 +- 1e-6

            net1.layers(1).weights(1, 0) shouldBe w110 +- 1e-6
            net1.layers(1).weights(1, 1) shouldBe w111 +- 1e-6
            net1.layers(1).weights(1, 2) shouldBe w112 +- 1e-6
            net1.layers(1).weights(1, 3) shouldBe w113 +- 1e-6

            net1.layers(0).weights(0, 0) shouldBe w000 +- 1e-6
            net1.layers(0).weights(0, 1) shouldBe w001 +- 1e-6
            net1.layers(0).weights(0, 2) shouldBe w002 +- 1e-6

            net1.layers(0).weights(1, 0) shouldBe w010 +- 1e-6
            net1.layers(0).weights(1, 1) shouldBe w011 +- 1e-6
            net1.layers(0).weights(1, 2) shouldBe w012 +- 1e-6

            net1.layers(0).weights(2, 0) shouldBe w020 +- 1e-6
            net1.layers(0).weights(2, 1) shouldBe w021 +- 1e-6
            net1.layers(0).weights(2, 2) shouldBe w022 +- 1e-6
          }
      }
  }

  property("net cost reduction") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "x"),
      (outGen, "y")) {
        (tdParms, x, y) =>
          {
            val net = TDNeuralNet(tdParms)(Seq(2, 3, 2))
            val status = net(x)
            val h = status.output
            val delta = y - h
            val cost = status.cost(delta)

            val net1 = status.train(delta)
            val status1 = net1(x)
            val h1 = status1.output
            val delta1 = y - h1
            val cost1 = status1.cost(delta1)

            cost1 should be <= cost
          }
      }
  }
}
