/**
 *
 */
package org.mmarini.actd

import breeze.linalg.DenseVector

/**
 * A status of environment
 *
 * Generates the input signal vector to the neural network and
 * produces the next status applying an action
 */
trait Status {
  /** Converts this status to a signal vector */
  def toDenseVector: DenseVector[Double]

  /** Returns the feedback for the given an action */
  def apply(action: Action): Feedback

  /** Returns true if this is a final status */
  def endEpisode: Boolean
}
