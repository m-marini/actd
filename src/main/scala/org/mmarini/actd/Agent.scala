/**
 *
 */
package org.mmarini.actd

import breeze.linalg.DenseVector

/**
 * A learning agent that replies to stimulus with actions and learns by receiving rewards
 *
 * [[TDAgent]] extends Agent
 */
trait Agent {
  /** Returns the action to be taken in a state */
  def action(status: Status): Int

  /** Returns a new agent that learns by the feedback */
  def train(feedback: Feedback): (Agent, Double)
}
