package org.mmarini.actd.samples

import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status
import breeze.linalg.DenseVector
import WallStatus._
import org.apache.commons.math3.random.MersenneTwister
import breeze.stats.distributions.RandBasis
import com.sun.javafx.webkit.theme.RenderThemeImpl.Widget
import org.mmarini.actd.Feedback

/** */
case class WallStatus(ball: (Int, Int), speed: (Int, Int), pad: Int) extends Status {

  import WallStatus.PadAction._

  /** */
  val toDenseVector: DenseVector[Double] = {
    val ballDim = Width * (Height + 1)
    val speedDim = 2
    val padDim = Width - PadSize + 1

    val v = DenseVector.zeros[Double](ballDim + speedDim + padDim)

    v.update(ball._1 * Width + ball._2, 1.0)

    v.update(ballDim + pad, 1.0)

    if (speed._1 > 0)
      v.update(ballDim + padDim, 1.0)

    if (speed._2 > 0)
      v.update(ballDim + padDim + 1, 1.0)

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
  def apply(action: Action) =
    if (finalStatus) {
      // Ball out of field
      Feedback(this, action, 0.0, initial)
    } else if (ball._1 < Height - 1 || speed._1 < 0) {
      // Ball in the field
      val (nb, ns) = hBounce(vBounce(ball, speed))
      Feedback(this, action, 0.0, WallStatus(nb, ns, movePad(action)))
    } else if (ball._2 == pad) {
      // left bounce ball
      val (nb, ns) = hBounce(vBounce((ball, (-1, -1))))
      Feedback(this, action, 1.0, WallStatus(nb, ns, movePad(action)))
    } else if (ball._2 == pad + 1) {
      // center bounce ball
      val (nb, ns) = hBounce(vBounce((ball, (-1, speed._2))))
      Feedback(this, action, 1.0, WallStatus(nb, ns, movePad(action)))
    } else if (ball._2 == pad + 2) {
      // right bounce ball
      val (nb, ns) = hBounce(vBounce((ball, (-1, 1))))
      Feedback(this, action, 1.0, WallStatus(nb, ns, movePad(action)))
    } else {
      val (nb, ns) = hBounce(vBounce(ball, speed))
      Feedback(this, action, -1.0, WallStatus(nb, ns, movePad(action)))
    }

  /** */
  override def finalStatus: Boolean = ball._1 >= Height

}

object WallStatus {

  val Width = 40
  val Height = 30

  val PadSize = 3

  val PadInitLocation = (Height - PadSize) / 2

  val random = new RandBasis(new MersenneTwister(1234l))

  /** MazeAction */
  object PadAction extends Enumeration {
    val Rest, Left, Right = Value

  }

  import PadAction._

  /** Creates a initial game status */
  def initial: WallStatus = WallStatus(newBall, newSpeed, PadInitLocation)

  /** */
  def newBall = (Height - 1, random.randInt(Width - PadSize).get)

  /** */
  def newSpeed = (-1, random.choose(Seq(-1, 1)).get)
}