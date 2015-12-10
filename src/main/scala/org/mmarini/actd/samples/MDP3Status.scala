/**
 *
 */
package org.mmarini.actd.samples

import org.mmarini.actd.Action
import org.mmarini.actd.Feedback
import org.mmarini.actd.Status

import breeze.linalg.DenseVector

/**
 * A sample environment status thats implement a deterministic MDP
 */
object MDP3Status {

  object MDP3Action extends Enumeration {
    val A0, A1 = Value
  }

  import MDP3Action._

  val PositiveReward = 1.0
  val NegativeReward = -1.0

  val S0: Status = new Status {
    val toDenseVector: DenseVector[Double] = DenseVector(1.0, 0.0)

    def apply(a: Action): Feedback = if (a == A0.id) {
      Feedback(this, a, NegativeReward, S0)
    } else {
      Feedback(this, a, NegativeReward, S1)
    }
  }

  val S1: Status = new Status {
    val toDenseVector: DenseVector[Double] = DenseVector(0.0, 1.0)

    def apply(a: Action): Feedback = if (a == A0.id) {
      Feedback(this, a, NegativeReward, S1)
    } else {
      Feedback(this, a, PositiveReward, S2)
    }
  }

  val S2: Status = new Status {
    val toDenseVector: DenseVector[Double] = DenseVector(0.0, 0.0)

    override val endEpisode = true

    def apply(a: Action): Feedback = Feedback(this, a, 0.0, S0)
  }
}
