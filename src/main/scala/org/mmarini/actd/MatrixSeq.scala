// Copyright (c) 2016 Marco Marini, marco.marini@mmarini.org
//
// Licensed under the MIT License (MIT);
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// https://opensource.org/licenses/MIT
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use,
// copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following
// conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.

package org.mmarini.actd

import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import breeze.stats.distributions.Rand
import breeze.stats.distributions.RandBasis
import breeze.linalg.csvwrite
import scalax.file.Path
import scalax.io.Resource
import java.io.File
/**
 * A container of [[ DenseMatrix[Double] ]] that applies algebraic operation *, +, -
 *
 * @constructor creates a [[MatrixSeq]] with a sequence of [[DenseMatrix[Double]]]
 * @param weights the sequence of matrixes
 *
 * @author us00852
 */
case class MatrixSeq(matrices: Seq[DenseMatrix[Double]]) {

  /** Returns the i-matrix */
  def apply(i: Int): DenseMatrix[Double] = matrices(i)

  /**  Returns the number of matrixes */
  def size: Int = matrices.size

  /** Returns a [[MatrixSeq]] with the matrixes scaled by a value */
  def *(a: Double): MatrixSeq =
    MatrixSeq(matrices.map(_ * a))

  /** Returns a [[MatrixSeq]] with the matrixes equals to the sum of matrixes */
  def +(a: MatrixSeq): MatrixSeq =
    MatrixSeq((matrices zip a.matrices).map {
      case (a, b) => a + b
    })

  /** Returns a [[MatrixSeq]] with the matrixes equals to the difference of matrixes */
  def -(a: MatrixSeq): MatrixSeq =
    MatrixSeq((matrices zip a.matrices).map {
      case (a, b) => a - b
    })

  /** Returns a [[Weight]] with zeros matrixes of the same size of original */
  def zeros: MatrixSeq =
    MatrixSeq(for { x <- matrices } yield DenseMatrix.zeros[Double](x.rows, x.cols))

  /** Returns the number of neuros for each layer */
  def layers: Seq[Int] =
    (matrices(0).cols - 1) +: matrices.map(_.rows)

  /** Converts to Seq */
  def toSeq: Seq[Double] =
    for {
      l <- matrices
      j <- 0 until l.cols
      i <- 0 until l.rows
    } yield l(i, j)

  /** Converts to [[DenseVector[Double]]] */
  def toDenseVector: DenseVector[Double] = DenseVector(toSeq.toArray)

  /** Writes to file */
  def write(file: String) {
    Seq(DenseVector(matrices.size.toDouble)).iterator.write(s"$file-n.csv")
    for {
      (m, i) <- matrices.zipWithIndex
    } {
      val fn = s"$file-$i.csv"
      Path.fromString(fn).deleteIfExists()
      csvwrite(new File(fn), m)
    }
  }
}

/**
 * Factory of [[MatrixSeq]] instances
 */
object MatrixSeq {

  /**
   * Create [[MatrixSeq]] with a hidden layer and random values
   */
  def rand(inputCount: Int, hiddenCount: Int, outputCount: Int, epsilon: Double): MatrixSeq =
    rand(Seq(inputCount, hiddenCount, outputCount), epsilon)

  /** Creates [[MatrixSeq]] with in-line values */
  def create(weights: DenseMatrix[Double]*): MatrixSeq = MatrixSeq(weights.toSeq)

  /** Creates [[MatrixSeq]] from DenseVector */
  def create(layers: Seq[Int], data: DenseVector[Double]): MatrixSeq = {
    val sizes = layers zip layers.tail
    val offsets = sizes.init.foldLeft(Seq(0))((seq, size) => {
      val (m, n) = size
      seq :+ (seq.head + n * (m + 1))
    })
    val w = for {
      (i, (m, n)) <- offsets zip sizes
    } yield data(i until i + n * (m + 1)).toDenseMatrix.reshape(n, m + 1)
    MatrixSeq(w)
  }

  /**
   * Create [[MatrixSeq]] with random values
   *
   * @param layers the number of neurons for each layer
   */
  def rand(layers: Seq[Int], rand: Rand[Double]): MatrixSeq =
    MatrixSeq(for {
      (n, m) <- layers.tail zip layers
    } yield {
      DenseMatrix.rand(n, m + 1, rand)
    })

  /**
   * Create [[MatrixSeq]] with random values
   *
   * @param layers the number of neurons for each layer
   */
  def rand(layers: Seq[Int], sigma: Double)(implicit randB: RandBasis = Rand): MatrixSeq =
    rand(layers, randB.gaussian(0.0, sigma))

  /**
   * create [[MatrixSeq]] with zero value
   *
   * @param layers the number of neurons for each layer
   */
  def zeros(layers: Seq[Int]): MatrixSeq =
    MatrixSeq(
      for {
        (n, m) <- layers.tail zip layers
      } yield DenseMatrix.zeros[Double](n, m + 1))
}
