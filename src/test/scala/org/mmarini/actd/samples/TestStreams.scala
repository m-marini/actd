/**
 *
 */
package org.mmarini.actd.samples

import com.typesafe.scalalogging.LazyLogging
import breeze.io.CSVWriter
import breeze.linalg.DenseMatrix
import breeze.linalg.DenseVector
import scalax.io.Resource
import scalax.file.Path

/**
 * Tests the maze environment
 * and generates a report of episode returns as octave data file
 */
object TestStreams extends LazyLogging {

  def trace[T](s: => Stream[T], msg: String = "", step: Int = 1): Stream[T] =
    s.zipWithIndex.map {
      case (x, i) =>
        if (i % step == 0) logger.info(f"$msg%s - $i%,d")
        x
    }

  /** Generates the report */
  def toMatrix(stream: => Stream[DenseVector[Double]]): DenseMatrix[Double] = {
    val out = stream.toArray
    DenseVector.horzcat(out: _*).t
  }

  def toCsvRows(stream: => Stream[DenseVector[Double]],
    separator: Char = ',',
    quote: Char = '\u0000',
    escape: Char = '\\'): Stream[String] =
    stream.map {
      case (mat) =>
        CSVWriter.mkString(
          IndexedSeq.tabulate(1, mat.size)((_, c) => mat(c).toString),
          separator, quote, escape)
    }

  def write(filename: String, stream: => Stream[DenseVector[Double]]) {

    Path.fromString(filename).deleteIfExists()
    val output = Resource.fromFile(filename)
    for {
      processor <- output.outputProcessor
      out = processor.asOutput
    } {
      for { row <- toCsvRows(stream) } {
        out.write(row)
        out.write("\n")
      }
    }
  }
}
