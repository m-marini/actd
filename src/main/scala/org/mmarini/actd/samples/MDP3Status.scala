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

package org.mmarini.actd.samples

import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status

import breeze.linalg.DenseVector

/**
 * A sample environment status thats implement a deterministic MDP
 */
object MDP3Status {

  object MDP3Action extends Enumeration {
    val A0, A1 = Value
  }

  import MDP3Action._

  val PositiveReward = 1.0
  val NegativeReward = -1.0

  val S0: Status = new Status {
    val toDenseVector: DenseVector[Double] = DenseVector(1.0, 0.0)

    def apply(a: Action): Feedback = if (a == A0.id) {
      Feedback(this, a, NegativeReward, S0)
    } else {
      Feedback(this, a, NegativeReward, S1)
    }
  }

  val S1: Status = new Status {
    val toDenseVector: DenseVector[Double] = DenseVector(0.0, 1.0)

    def apply(a: Action): Feedback = if (a == A0.id) {
      Feedback(this, a, NegativeReward, S1)
    } else {
      Feedback(this, a, PositiveReward, S2)
    }
  }

  val S2: Status = new Status {
    val toDenseVector: DenseVector[Double] = DenseVector(0.0, 0.0)

    override def finalStatus = true

    def apply(a: Action): Feedback = Feedback(this, a, 0.0, S0)
  }
}
