/**
 *
 */
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

  /** Returns a random action from the available actions */
  def action(status: Status): Action = actionRand.sample

  /** Returns this agent (no learning algorithm is implemented for dummy agent) */
  def train(feedback: Feedback): (Agent, Double) = (this, 0.0)

}

/** Factory for [[DummyAgent]] instances */
object DummyAgent {

  /** Creates a [[DummyAgent]] with actionCount available actions and a random basis */
  def apply(actionCount: Int, random: RandBasis = Rand): DummyAgent =
    new DummyAgent(random.randInt(actionCount))
}
