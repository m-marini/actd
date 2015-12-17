package org.mmarini.actd.samples

import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status

import breeze.linalg.DenseVector

/** */
case class GraphStatus(s: Int, size: () => Int, next: (Int, Action) => (Int, Double), isFinalStatus: Int => Boolean) extends Status {

  require(s >= 0 && s < size())

  /** */
  val toDenseVector: DenseVector[Double] = {
    val v = DenseVector.zeros[Double](size())
    v.update(s, 1.0)
    v
  }

  /** */
  def apply(action: Action) = {
    val (s1, reward) = next(s, action)
    val f = Feedback(this, action, reward, GraphStatus(s1, size, next, isFinalStatus))
    f
  }

  /** */
  override def finalStatus = isFinalStatus(s)
}

object GraphStatus {

  /** MazeAction */
  object MazeAction extends Enumeration {
    val Up, Down, Left, Right = Value

  }

  import MazeAction._

  /**
   * Creates a flat field maze with given size
   */
  def flatFieldMaze(rows: Int, cols: Int): GraphStatus = {
    val worldSize = rows * cols
    val targets = Set(worldSize - 1)
    val InitStatus = 0
    val WrongActionReward = -1.0

    def toCoord(s: Int) = (s / cols, s % cols)

    def toStatus(row: Int, col: Int) = row * cols + col

    def next(s: Int, a: Action): Option[Int] = {
      val (r, c) = toCoord(s)
      val nxt = MazeAction(a) match {
        case Up if (r > 0) => Some(toStatus(r - 1, c))
        case Down if (r < rows - 1) => Some(toStatus(r + 1, c))
        case Left if (c > 0) => Some(toStatus(r, c - 1))
        case Right if (c < cols - 1) => Some(toStatus(r, c + 1))
        case _ => None
      }
      nxt
    }

    def apply(s: Int, a: Action): (Int, Double) = if (targets.contains(s)) {
      (InitStatus, 0.0)
    } else {
      val s1 = next(s, a).getOrElse(s)
      (s1, if (targets.contains(s1)) 1.0 else WrongActionReward)
    }

    GraphStatus(InitStatus, () => worldSize, apply, targets.contains)
  }
}