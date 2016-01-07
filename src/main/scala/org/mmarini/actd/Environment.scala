/**
 *
 */
package org.mmarini.actd

import scala.collection.Iterator

/**
 * An environment with a [[Status]] and an [[Agent]]
 *
 * @constructor creates an environment with a status and an agent
 *
 * @param status the status
 * @param agent the agent
 *
 * @author us00852
 */
case class Environment(status: Status, agent: Agent) {

  /**
   * Steps over the status creating the feedback
   *
   *  @return the feedback
   */
  def stepOver: (Environment, Environment, Feedback) = {
    val action = agent.action(status)
    val feedback = status(action)
    val agent1 = agent.learn(feedback)
    (this, Environment(feedback.s1, agent1), feedback)
  }

  /** Converts this Environment into a stream of environments, rewards and end episode flags */
  def iterator: Iterator[(Environment, Environment, Feedback)] =
    Iterator.iterate(stepOver)(_._2.stepOver)
}
