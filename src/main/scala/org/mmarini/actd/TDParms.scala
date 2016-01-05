/**
 *
 */
package org.mmarini.actd

import scala.util.Random
import breeze.linalg.DenseVector
import breeze.linalg.DenseVector
import breeze.numerics.exp
import breeze.linalg.argmax
import breeze.linalg.sum
import breeze.stats.distributions.RandBasis
import breeze.stats.distributions.Bernoulli

/**
 * A set of TD parameter
 *
 * @param alpha regularization network parameter, 0 no regularization, 1 no learning
 * @param beta action update rate parameter
 * @param gamma reward discount parameter
 * @param lambda tracebilty parameter of TD(lambda) algorithm
 * @param eta learning rate parameter of neural network
 * @param random
 */
case class TDParms(
    alpha: Double,
    beta: Double,
    gamma: Double,
    epsilon: Double,
    lambda: Double,
    eta: Double,
    random: RandBasis) {

  private val egreedyRand = new Bernoulli(epsilon, random)

  /** Returns a [[TDParms]] with changed eta value */
  def setEta(value: Double): TDParms =
    TDParms(alpha = alpha,
      beta = beta,
      gamma = gamma,
      epsilon = epsilon,
      lambda = lambda,
      eta = value,
      random = random)

  /** Returns a index with uniform distribution with epsilon probability otherwise by softmax algorithm */
  def indexEGreedyBySoftmax(pref: DenseVector[Double]): Int =
    if (egreedyRand.sample) {
      random.randInt(pref.length).sample
    } else {
      indexByWeights(exp(pref))
    }

  /** Returns a random index with by softmax algorithm */
  def indexBySoftmax(pref: DenseVector[Double]): Int =
    indexByWeights(exp(pref))

  /** Returns a random index by weights */
  def indexByWeights(pref: DenseVector[Double]): Int = {
    val acc = pref.toArray.foldLeft(List(0.0)) {
      case (r, x) => (x + r.head) :: r
    }
    val x = random.uniform.sample * sum(pref)
    acc.reverse.tail.indexWhere(x < _)
  }

}
