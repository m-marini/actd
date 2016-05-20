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

import breeze.linalg.DenseVector
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Rand

/**
 * A dummy agent that replies to stimulus with random actions and learns nothing.
 *
 * Because the [[DummyAgent]] generate strategy based just on random actions it is used
 * to generate benchmark KPIs.
 *
 *
 * @constructor create a dummy agent with a random action generator
 * @param actionRand the action randomizer
 *
 * @author us00852
 */
class DummyAgent(actionRand: Rand[Action]) extends Agent {

  def parms: TDParms = throw new IllegalArgumentException

  /** Returns a random action from the available actions */
  def action(status: Status): Action = actionRand.sample

  /** Returns this agent (no learning algorithm is implemented for dummy agent) */
  def train(feedback: Feedback): (Agent, Double) = (this, 0.0)

  def write(file: String) {}
}

/** Factory for [[DummyAgent]] instances */
object DummyAgent {

  /** Creates a [[DummyAgent]] with actionCount available actions and a random basis */
  def apply(actionCount: Int, random: RandBasis = Rand): DummyAgent =
    new DummyAgent(random.randInt(actionCount))
}
