/**
 *
 */
package org.mmarini.actd

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

/**
 * A neural network that creates the network status for a given input and
 * creates new [[Net]] by learning from an input vector and and expected output vector
 *
 * @author us00852
 */
trait NeuralNet {
  /**  Generates next learning status given an input and the expected output */
  def learn(in: DenseVector[Double], expected: DenseVector[Double]): NeuralNet

  /** Returns the network status */
  def apply(in: DenseVector[Double]): NetStatus
}
