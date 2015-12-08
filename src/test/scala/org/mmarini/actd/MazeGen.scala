/**
 *
 */
package org.mmarini.actd

import breeze.linalg.DenseVector
import breeze.linalg.DenseMatrix
import org.scalacheck.Gen

object MazeGen {

  def vector(n: Int, gen: Gen[Double]): Gen[DenseVector[Double]] =
    Gen.listOfN(n, gen).map(s => DenseVector(s.toArray))

  def vector(n: Int, range: Double): Gen[DenseVector[Double]] = vector(n, Gen.choose(-range, range))

  def matrix(n: Int, m: Int, gen: Gen[Double]): Gen[DenseMatrix[Double]] =
    Gen.listOfN(n * m, gen).map(s => new DenseMatrix(n, m, s.toArray))

  def matrix(n: Int, m: Int, range: Double): Gen[DenseMatrix[Double]] =
    matrix(n, m, Gen.choose(-range, range))

  def weights(w0Gen: Gen[DenseMatrix[Double]], w1Gen: Gen[DenseMatrix[Double]]): Gen[MatrixSeq] = for {
    w0 <- w0Gen
    w1 <- w1Gen
  } yield MatrixSeq.create(w0, w1)
}
