/**
 *
 */
package org.mmarini.actd.samples

import org.apache.commons.math3.random.MersenneTwister
import org.mmarini.actd.Feedback
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status
import com.typesafe.scalalogging.LazyLogging
import WallStatus.Width
import WallStatus.Height
import WallStatus.PadSize
import WallStatus.PadAction
import WallStatus.PositiveReward
import WallStatus.NegativeReward
import breeze.linalg.DenseVector
import breeze.stats.distributions.RandBasis
import org.mmarini.actd.Action

/** */
case class WallStatus(ball: (Int, Int), speed: (Int, Int), pad: Int) extends Status {

  import PadAction._

  /** */
  val toDenseVector: DenseVector[Double] = {
    val ballDim = Width * (Height + 1)
    val speedDim = 2
    val padDim = Width - PadSize + 1

    val v = DenseVector.zeros[Double](ballDim + speedDim + padDim)

    v.update(ball._1 * Width + ball._2, 1.0)

    v.update(ballDim + pad, 1.0)

    if (speed._1 > 0) {
      v.update(ballDim + padDim, 1.0)
    }
    if (speed._2 > 0) {
      v.update(ballDim + padDim + 1, 1.0)
    }
    v
  }

  /** */
  private def hBounce(x: ((Int, Int), (Int, Int))) = {
    val ((r, c), (sr, sc)) = x
    (c + sc) match {
      case -1 => ((r, 1), (sr, 1))
      case Width => ((r, Width - 2), (sr, -1))
      case c => ((r, c), (sr, sc))
    }
  }

  /** */
  private def vBounce(x: ((Int, Int), (Int, Int))) = {
    val ((r, c), (sr, sc)) = x
    (r + sr) match {
      case -1 => ((1, c), (1, sc))
      case r => ((r, c), (sr, sc))
    }
  }

  /** */
  private def movePad(action: Action) = PadAction.apply(action) match {
    case Left if pad > 0 => pad - 1
    case Right if pad < Width - PadSize => pad + 1
    case _ => pad
  }

  /** */
  def apply(action: Action): Feedback =
    if (finalStatus) {
      // Restarts because ball is out of field
      Feedback(this, action, 0.0, WallStatus.initial)
    } else if (ball._1 < Height - 1 || speed._1 < 0) {
      // Ball in the field
      val (nb, ns) = hBounce(vBounce(ball, speed))
      Feedback(this, action, 0.0, WallStatus(nb, ns, movePad(action)))
    } else if (ball._2 == pad) {
      // left bounce ball
      val (nb, ns) = hBounce(vBounce((ball, (-1, -1))))
      Feedback(this, action, PositiveReward, WallStatus(nb, ns, movePad(action)))
    } else if (ball._2 == pad + 1) {
      // center bounce ball
      val (nb, ns) = hBounce(vBounce((ball, (-1, speed._2))))
      Feedback(this, action, PositiveReward, WallStatus(nb, ns, movePad(action)))
    } else if (ball._2 == pad + 2) {
      // right bounce ball
      val (nb, ns) = hBounce(vBounce((ball, (-1, 1))))
      Feedback(this, action, PositiveReward, WallStatus(nb, ns, movePad(action)))
    } else {
      val (nb, ns) = hBounce(vBounce(ball, speed))
      Feedback(this, action, NegativeReward, WallStatus(nb, ns, movePad(action)))
    }

  /** */
  override def finalStatus: Boolean = ball._1 >= Height

}

object WallStatus extends LazyLogging {
  val Seed = 123L

  val Height = 10
  val Width = 13

  val PadSize = 3

  val PositiveReward = 1.0

  val NegativeReward = -PositiveReward - PositiveReward

  val random = new RandBasis(new MersenneTwister(Seed))

  /** MazeAction */
  object PadAction extends Enumeration {
    val Rest, Left, Right = Value

  }

  import PadAction._

  /** Creates a initial game status */
  def initial: WallStatus = {
    val b = (Height - 1, random.randInt(Width).get)
    val s = (-1, random.choose(Seq(-1, 1)).get)
    val pad = b._2 match {
      case 0 => 0
      case c if (c - 1 >= Width - PadSize) => Width - PadSize
      case c => c - 1
    }
    WallStatus(b, s, pad)
  }
}
