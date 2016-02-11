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

import org.scalamock.scalatest.MockFactory
import org.scalatest.FunSpec
import org.scalatest.GivenWhenThen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks

import breeze.linalg.DenseVector

class EnvironmentTest extends FunSpec with PropertyChecks with Matchers with GivenWhenThen with MockFactory {

  describe("An environment") {
    val statusVect = DenseVector(1.0)
    val expReward = 10.0
    val expAction = 0

    describe("when stepOver to non final status") {

      it("""should return the new environment with next status
  and the reward by status
  and the end episode to false
  and should call once apply to status
  and call once action to agent
  and call once learn to agent""") {

        val status = mock[Status]
        val agent = mock[Agent]
        val expNext = mock[Status]

        val feedback = Feedback(status, expAction, expReward, expNext)

        // Mocking status behavior
        (status.toDenseVector _).expects.anyNumberOfTimes.returns(statusVect)
        (status.apply _).expects(expAction).returns(feedback)

        // Mocking agent behavior
        (agent.action _).expects(status).returns(expAction)
        (agent.train _).expects(feedback).returns((agent, 0.0))

        (expNext.toDenseVector _).expects.anyNumberOfTimes.returns(statusVect)

        val env = Environment(status, agent)

        val (_, next, Feedback(_, _, reward, _), _) = env.stepOver

        next should have('status(expNext))
        reward should be(expReward)
      }
    }

    describe("when converted to stream of next 2 state") {

      it("""should have head with first status change""") {

        val s0 = mock[Status]
        val s1 = mock[Status]
        val s2 = mock[Status]

        val a0 = mock[Agent]
        val a1 = mock[Agent]
        val a2 = mock[Agent]

        val r0 = -1.0
        val r1 = 1.0

        val f0 = Feedback(s0, 1, r0, s1)
        val f1 = Feedback(s1, 1, r1, s2)

        // Mocking status 0 behavior
        (s0.toDenseVector _).expects.anyNumberOfTimes.returns(statusVect)
        (s0.finalStatus _).expects.anyNumberOfTimes.returns(false)
        (s0.apply _).expects(*).returns(f0)

        // Mocking status 1 behavior
        (s1.toDenseVector _).expects.anyNumberOfTimes.returns(statusVect)
        (s1.finalStatus _).expects.anyNumberOfTimes.returns(false)
        (s1.apply _).expects(*).returns(f1)

        // Mocking status 1 behavior
        (s2.toDenseVector _).expects.anyNumberOfTimes.returns(statusVect)
        (s2.finalStatus _).expects.anyNumberOfTimes.returns(true)

        // Mocking agent 0 behavior
        (a0.action _).expects(*).returns(1)
        (a0.train _).expects(*).returns((a1, 0.0))

        // Mocking agent 1 behavior
        (a1.action _).expects(*).returns(1)
        (a1.train _).expects(*).returns((a2, 0.0))

        val iter = Environment(s0, a0).iterator

        val (_, next, Feedback(_, _, reward, _), _) = iter.next

        next should have('status(s1))
        reward should be(r0)

        val (_, next1, Feedback(_, _, reward1, _), _) = iter.next

        next1 should have('status(s2))
        reward1 should be(r1)
      }
    }
  }
}
