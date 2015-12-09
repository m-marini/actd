/**
 *
 */
package org.mmarini.actd.samples

import CellStatus.Me
import CellStatus.Target
import CellStatus.Wall
import MazeAction.Down
import MazeAction.Left
import MazeAction.Right
import MazeAction.Up
import MazeStatus.NegativeReward
import MazeStatus.NoneReward
import MazeStatus.PositiveReward
import breeze.linalg.DenseVector
import org.mmarini.actd.Status
import org.mmarini.actd.Feedback
import org.mmarini.actd.Action
import org.mmarini.actd.Feedback

/** CellStatus */
object CellStatus extends Enumeration {
  val Me, Target, Wall = Value
}

/** MazeAction */
object MazeAction extends Enumeration {
  val Up, Down, Left, Right = Value

}

/**
 * A state of maze with position of me, targets and walls
 *
 *  @param size the size of world row * columns
 *  @param me my location
 *  @param target the target location
 *  @param walls the wall location
 */
case class MazeStatus(rows: Int, cols: Int, me: Int, target: Int, walls: Set[Int]) extends Status {

  require(rows > 0)
  require(cols > 0)

  /** Compute the location for a move */
  def move(action: MazeAction.Value)(from: Int): Option[Int] = {
    val (row, col) = coords(from)
    action match {
      case Up if (row > 0) => Some(from - cols)
      case Down if (row < rows - 1) => Some(from + cols)
      case Left if (col > 0) => Some(from - 1)
      case Right if (col < cols - 1) => Some(from + 1)
      case _ => None
    }
  }

  /** Returns the row and column coordinate of a location */
  def coords(location: Int): (Int, Int) = (location / cols, location % cols)

  /**
   * Returns the status of a cell
   *
   *  @param key the cell coordinate
   */
  def status(key: Int): Option[CellStatus.Value] = {
    require(inside(key))
    if (walls.contains(key)) {
      Some(Wall)
    } else if (key == me) {
      Some(Me)
    } else if (key == target) {
      Some(Target)
    } else {
      None
    }
  }

  /** Returns true if the location is inside the maze */
  private def inside(l: Int): Boolean = l >= 0 && l < rows * cols

  /**
   * Applies an action to the state
   *
   * @param action the action
   * @return the new status, the reward and the flag of end episode
   */
  def apply(action: Action): Feedback = {
    if (endEpisode) {
      Feedback(this, action, 0.0, MazeStatus.init)
    } else {
      val x = for {
        me1 <- move(MazeAction.apply(action))(me)
      } yield {
        val reward = if (me1 == target) PositiveReward else NoneReward
        val s1 = if (endEpisode) MazeStatus.init else MazeStatus(rows, cols, me1, target, walls)
        Feedback(this, action, reward, s1)
      }
      x.getOrElse(Feedback(this, action, NegativeReward, this))
    }
  }

  /** Returns the status as a DenseVector */
  lazy val toDenseVector: DenseVector[Double] = {
    val v = for {
      s <- CellStatus.values.toSeq.sorted
      i <- 0 until rows * cols
    } yield if (status(i) == s) 1.0 else 0.0
    DenseVector(v.toArray)
  }

  /** Returns true if I have reached the target */
  override def endEpisode: Boolean = me == target
}

/**
 *
 */
object MazeStatus {
  val PositiveReward = 1.0
  val NegativeReward = -1.0
  val NoneReward = -1.0
  val WorldHeight = 5
  val WorldWidth = 5
  val WorldSize = WorldHeight * WorldWidth

  /** The initial status of maze */
  val init: MazeStatus =
    MazeStatus(WorldHeight, WorldWidth, 0, WorldSize - 1, Set())
}
