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

/**
 * @author us00852
 */
class TDLayerTest extends PropSpec with PropertyChecks with Matchers with GivenWhenThen {

  val InputRange = 1e2
  val OutputRange = 1e2

  val inGen = MazeGen.vector(2, InputRange)
  val nlrOutGen = MazeGen.vector(2, OutputRange)
  val hiddenOutGen = MazeGen.vector(2, 1.0)

  val tdParmsGen = for {
    alpha <- Gen.choose(0.0, 1.0)
    gamma <- Gen.choose(0.0, 1.0)
    lambda <- Gen.choose(0.0, 1.0)
    eta <- Gen.choose(0.0, 1e-6)
  } yield TDParms(
    alpha = alpha,
    beta = 0.0,
    gamma = gamma,
    epsilon = 0.0,
    lambda = lambda,
    eta = eta,
    random = Rand)

  property("nlr output") {

    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in")) {
        (tdParms, in) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.nlr(tdParms))
            val status = layer(in)
            val out = status.output
            val o0 = layer.weights(0, 0) + layer.weights(0, 1) * in(0) + layer.weights(0, 2) * in(1)
            val o1 = layer.weights(1, 0) + layer.weights(1, 1) * in(0) + layer.weights(1, 2) * in(1)

            out(0) shouldBe o0 +- 1e-6
            out(1) shouldBe o1 +- 1e-6
          }
      }
  }

  property("nlr cost") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (nlrOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.nlr(tdParms))
            val status = layer(in)
            val e = exp - status.output
            val cost0 = status.cost(e)
            val alpha = tdParms.alpha
            val w = layer.weights
            val e2 = e :* e
            val w2 = w :* w
            val expectedCost = ((1 - alpha) * (e2(0) + e2(1)) +
              alpha * (w2(0, 1) + w2(0, 2) + w2(1, 1) + w2(1, 2))) / 2
            cost0 shouldBe expectedCost +- 1e-6
          }
      }
  }

  property("nlr trained layer") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (nlrOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.nlr(tdParms))
            val status = layer(in)
            val e = exp - status.output
            val (next, _) = status.train(e)
            val alpha = tdParms.alpha
            val eta = tdParms.eta
            val w = layer.weights

            val dj00 = -(1 - alpha) * e(0)
            val dj01 = -(1 - alpha) * e(0) * in(0) + alpha * w(0, 1)
            val dj02 = -(1 - alpha) * e(0) * in(1) + alpha * w(0, 2)
            val dj10 = -(1 - alpha) * e(1)
            val dj11 = -(1 - alpha) * e(1) * in(0) + alpha * w(1, 1)
            val dj12 = -(1 - alpha) * e(1) * in(1) + alpha * w(1, 2)

            val w00 = w(0, 0) - eta * dj00
            val w01 = w(0, 1) - eta * dj01
            val w02 = w(0, 2) - eta * dj02
            val w10 = w(1, 0) - eta * dj10
            val w11 = w(1, 1) - eta * dj11
            val w12 = w(1, 2) - eta * dj12

            val expTraces = DenseMatrix(
              (-dj00, -dj01, -dj02),
              (-dj10, -dj11, -dj12))
            val expWeights = DenseMatrix(
              (w00, w01, w02),
              (w10, w11, w12))

            next.parms shouldBe layer.parms
            TestFuncs.matrixLike(next.traces, expTraces, 1e-6)
            TestFuncs.matrixLike(next.weights, expWeights, 1e-6)
          }
      }
  }

  property("nlr backError") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (nlrOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.nlr(tdParms))
            val status = layer(in)
            val e = exp - status.output
            val (_, backError) = status.train(e)
            val alpha = tdParms.alpha
            val w = layer.weights
            val exp0 = e(0) * w(0, 1) + e(1) * w(1, 1)
            val exp1 = e(0) * w(0, 2) + e(1) * w(1, 2)

            backError(0) shouldBe exp0 +- 1e-6
            backError(1) shouldBe exp1 +- 1e-6
          }
      }
  }

  property("nlr cost reduction") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (nlrOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.nlr(tdParms))
            val status = layer(in)
            val e = exp - status.output
            val (layer1, _) = status.train(e)
            val cost0 = status.cost(e)

            val status1 = layer1(in)
            val e1 = exp - status1.output
            val cost1 = status1.cost(e1)

            cost1 should be <= cost0
          }
      }
  }

  property("hidden output") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in")) {
        (tdParms, in) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.hidden(tdParms))
            val status = layer(in)
            val out = status.output
            val exp0 = tanh(layer.weights(0, 0) + layer.weights(0, 1) * in(0) + layer.weights(0, 2) * in(1))
            val exp1 = tanh(layer.weights(1, 0) + layer.weights(1, 1) * in(0) + layer.weights(1, 2) * in(1))

            out(0) shouldBe exp0 +- 1e-6
            out(1) shouldBe exp1 +- 1e-6
          }
      }
  }

  property("hidden cost") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (hiddenOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.hidden(tdParms))
            val status = layer(in)
            val e = exp - status.output
            val cost0 = status.cost(e)
            val alpha = tdParms.alpha
            val w = layer.weights
            val e2 = e :* e
            val w2 = w :* w
            val expectedCost = ((1 - alpha) * (e2(0) + e2(1)) +
              alpha * (w2(0, 1) + w2(0, 2) + w2(1, 1) + w2(1, 2))) / 2
            cost0 shouldBe expectedCost +- 1e-6
          }
      }
  }

  property("hidden trained layer") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (hiddenOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.hidden(tdParms))
            val status = layer(in)
            val h = status.output
            val e = exp - h
            val (next, _) = status.train(e)

            next.parms shouldBe layer.parms

            val alpha = tdParms.alpha
            val eta = tdParms.eta
            val w = layer.weights

            val dj00 = -(1 - alpha) * e(0) * (1 + h(0)) * (1 - h(0))
            val dj01 = -(1 - alpha) * e(0) * (1 + h(0)) * (1 - h(0)) * in(0) + alpha * w(0, 1)
            val dj02 = -(1 - alpha) * e(0) * (1 + h(0)) * (1 - h(0)) * in(1) + alpha * w(0, 2)
            val dj10 = -(1 - alpha) * e(1) * (1 + h(1)) * (1 - h(1))
            val dj11 = -(1 - alpha) * e(1) * (1 + h(1)) * (1 - h(1)) * in(0) + alpha * w(1, 1)
            val dj12 = -(1 - alpha) * e(1) * (1 + h(1)) * (1 - h(1)) * in(1) + alpha * w(1, 2)

            val e00 = -signum(dj00)
            val e01 = -signum(dj01)
            val e02 = -signum(dj02)
            val e10 = -signum(dj10)
            val e11 = -signum(dj11)
            val e12 = -signum(dj12)

            next.traces(0, 0) shouldBe e00 +- 1e-6
            next.traces(0, 1) shouldBe e01 +- 1e-6
            next.traces(0, 2) shouldBe e02 +- 1e-6
            next.traces(1, 0) shouldBe e10 +- 1e-6
            next.traces(1, 1) shouldBe e11 +- 1e-6
            next.traces(1, 2) shouldBe e12 +- 1e-6

            val w00 = w(0, 0) + eta * e00
            val w01 = w(0, 1) + eta * e01
            val w02 = w(0, 2) + eta * e02
            val w10 = w(1, 0) + eta * e10
            val w11 = w(1, 1) + eta * e11
            val w12 = w(1, 2) + eta * e12

            next.weights(0, 0) shouldBe w00 +- 1e-6
            next.weights(0, 1) shouldBe w01 +- 1e-6
            next.weights(0, 2) shouldBe w02 +- 1e-6
            next.weights(1, 0) shouldBe w10 +- 1e-6
            next.weights(1, 1) shouldBe w11 +- 1e-6
            next.weights(1, 2) shouldBe w12 +- 1e-6
          }
      }
  }

  property("hidden backError") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (hiddenOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.hidden(tdParms))
            val status = layer(in)
            val h = status.output
            val e = exp - h
            val (_, backError) = status.train(e)
            val alpha = tdParms.alpha
            val w = layer.weights
            val exp0 = e(0) * (1 + h(0)) * (1 - h(0)) * w(0, 1) + e(1) * (1 + h(1)) * (1 - h(1)) * w(1, 1)
            val exp1 = e(0) * (1 + h(0)) * (1 - h(0)) * w(0, 2) + e(1) * (1 + h(1)) * (1 - h(1)) * w(1, 2)

            backError(0) shouldBe exp0 +- 1e-6
            backError(1) shouldBe exp1 +- 1e-6
          }
      }
  }

  property("hidden cost reduction") {
    forAll(
      (tdParmsGen, "tdParms"),
      (inGen, "in"),
      (hiddenOutGen, "exp")) {
        (tdParms, in, exp) =>
          {
            val layer = TDLayer(2, 2, TDLayerParms.hidden(tdParms))
            val status = layer(in)
            val e = exp - status.output
            val (layer1, _) = status.train(e)
            val cost0 = status.cost(e)

            val status1 = layer1(in)
            val e1 = exp - status1.output
            val cost1 = status1.cost(e1)

            cost1 should be <= cost0
          }
      }
  }
}
