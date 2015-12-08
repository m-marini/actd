/**
 *
 */
package org.mmarini.actd

import breeze.linalg.DenseVector

/**
 * A Feedback that stores the previous status, the action taken, the target status,
 * the reward received and the flag signaling the end of episode
 */
case class Feedback(s0: Status, action: Action, reward: Double, s1: Status)
