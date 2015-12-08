/**
 *
 */
package org.mmarini.actd

import org.scalatest.Matchers._
import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix

object TestFuncs {
  def vectorLike(v: DenseVector[Double], expected: DenseVector[Double], epsilon: Double = 1e-10) {
    v.length should be(expected.length)
    for { i <- 0 until v.length } {
      v(i) should be(expected(i) +- epsilon)
    }
  }

  def matrixLike(v: DenseMatrix[Double], expected: DenseMatrix[Double], epsilon: Double = 1e-10) {
    v.rows should be(expected.rows)
    v.cols should be(expected.cols)
    for {
      i <- 0 until v.rows
      j <- 0 until expected.cols
    } {
      v(i, j) should be(expected(i, j) +- epsilon)
    }
  }
}
