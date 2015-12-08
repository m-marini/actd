/**
 *
 */
package org.mmarini.actd

import breeze.linalg.{ * => * }
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import com.typesafe.scalalogging.LazyLogging

/**
 * A status of Non Linear Regression neural network.
 * .
 * @constructor create a status with weights and layers values
 * @param weights the synapse weights
 * @param layers the neuro state
 *
 * @author us00852
 */
class NLRStatus(
    val weights: MatrixSeq,
    val layers: Seq[DenseVector[Double]]) extends NetStatus {

  /** Computes the back propagation tensor */
  def bTensor: MatrixSeq = {
    val bt = for {
      (h, w) <- layers.tail.init zip weights.matrices.tail
    } yield {
      val n = h.length
      val hh = h(1 to -1)
      val dz = hh :* (DenseVector.ones[Double](n - 1) - hh)
      val w0 = w(::, 1 to -1)
      val b = w0(*, ::) :* dz
      b.t
    }
    MatrixSeq(bt)
  }

}
