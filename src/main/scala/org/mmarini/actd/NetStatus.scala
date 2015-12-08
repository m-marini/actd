/**
 *
 */
package org.mmarini.actd

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg._
import breeze.linalg.*
import breeze.linalg.DenseVector

/**
 * A status of general neural network.
 *
 * Computes output, the cost function, the gradient of cost function the back propagation tensor.
 *
 * [[NLRStatus]] implements [[NetStatus]]
 *
 * @author us00852
 */
trait NetStatus {
  /**
   * Returns the network weights in the form of [[MatrixSeq]] by layer.
   * The first matrix is related to input layer the last one to the output layer.
   */
  def weights: MatrixSeq

  /**
   * Returns the layer values as a in the form of IndexedSeq of Vector.
   * The first layer is the input layer and the last one is the output layer
   * Each layer vector contains the bias unit value as the first component: layer(i)(0) = 1.0
   */
  def layers: Seq[DenseVector[Double]]

  /** Generates the back propagation tensor  */
  def bTensor: MatrixSeq

  /** Computes the output */
  lazy val output: DenseVector[Double] = layers.last(1 to -1)

  /**
   * Generates the back propagated errors
   * delta(ij) = sum_k delta((i+1)k) * b(i+1)(k,j)
   */
  def bError(error: DenseVector[Double]): Seq[DenseVector[Double]] = {
    val l = bTensor.matrices.reverse
    l.foldLeft(Seq(error))((errs, b) => {
      val delta = errs.head
      val e = b * delta
      e +: errs
    })
  }

  /**
   * Generates the gradient for a given error.
   * grad(ijk) = - [(1 - alpha) (delta(ij) h(ik)) + alpha w(ijk)]
   * grad(ij0) = - eta * [(1 - alpha) (delta(ij) h(ij))]
   *
   * @param error the output error
   * @param alpha the regularization factor
   */
  def gradient(error: DenseVector[Double], alpha: Double): MatrixSeq = {
    val be = bError(error)
    MatrixSeq(for {
      ((delta, h), w) <- be zip layers zip weights.matrices
    } yield {
      val dw1 = (delta * h.t) * (alpha - 1)
      val dw2 = DenseMatrix.horzcat(DenseMatrix.zeros[Double](dw1.rows, 1), w(::, 1 to -1)) * alpha
      val dw = dw1 + dw2
      dw
    })
  }

  /** Returns the cost */
  def cost(expected: DenseVector[Double], alpha: Double): Double = {
    val error = expected - output
    ((1 - alpha) * (error.t * error) + alpha * weights.matrices.map(m => sum(m :* m)).sum) / 2
  }
}
