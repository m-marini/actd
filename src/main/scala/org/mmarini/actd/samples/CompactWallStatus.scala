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

package org.mmarini.actd.samples

import scala.math.sqrt

import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status

import com.typesafe.scalalogging.LazyLogging

import breeze.linalg.DenseVector
import breeze.linalg.InjectNumericOps

/** Factory of [[WallStatusDeep]] */
object CompactWallStatus extends LazyLogging {

  import WallStatus._

  object StatusIndex extends Enumeration {
    val BallColumIdx, BallRowIdx, BallDrectionIdx, PadIdx, FinalIdx = Value
  }
  val InputSpaceDimension = StatusIndex.maxId

  private val MinValues = DenseVector[Double](0, 0, 0, 0, -1)
  private val MaxValues = DenseVector[Double](Height + 1, Width, Direction.maxId, LastPad, 1)
  private val Scale = sqrt(3) * 2 * DenseVector.ones[Double](InputSpaceDimension) :/ (MaxValues - MinValues)
  private val Mean = (MaxValues - MinValues) / 2.0

  val FinalVector = normalizeStatus(DenseVector[Double](
    0,
    0,
    0,
    0,
    1))

  FinalVector.update(0, 1.0)

  private def normalizeStatus(x: DenseVector[Double]): DenseVector[Double] = Scale :* (x - Mean)

  /**
   * Transforms the status into a Vector
   * The resulting vector consists of (rows x columns) signals for the ball position,
   * 4 signals for the ball direction,
   * (columns - padsize + 1) signals for pad location
   * and a signal for final status
   */
  def toDenseVector(status: WallStatus): DenseVector[Double] = {
    if (status.finalStatus) {
      FinalVector
    } else {
      normalizeStatus(DenseVector[Double](
        status.ball._1,
        status.ball._2,
        status.direction.id,
        status.pad,
        -1))
    }
  }
}
