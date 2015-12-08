/**
 *
 */
package org.mmarini.actd

import breeze.linalg.DenseVector

/**
 * A learning agent that replies to stimulus with actions and learns by receiving rewards
 * applying TD algorithm.
 *
 * @constructor create a learning agent with parameters, critic network and actor network
 * @parm parms the parameters
 * @parm critic the critic network
 * @parm actor the actor network
 *
 * @author us00852
 */
class TDAgent(
    val parms: TDParms,
    val critic: TDNeuralNet,
    val actor: TDNeuralNet) extends Agent {

  /** Returns the action to be taken in a state */
  def action(status: Status): Action =
    //        parms.indexByGreedy(actor(status).output)
    parms.indexBySoftmax(actor(status.toDenseVector).output)

  /** Returns a new agent that learns by reward */
  def learn(feedback: Feedback): TDAgent = {

    // Computes the state value pre and post step
    val s0Vect = feedback.s0.toDenseVector
    val preValue = critic(s0Vect).output(0)
    val s1Vect = feedback.s1.toDenseVector

    val endEpisode = feedback.s1.endEpisode

    // The status value of post state is 0 if final episode else bootstraps from critic
    val postValue = if (endEpisode) 0.0 else critic(s1Vect).output(0)

    // Computes the expected state value by booting the previous status value */
    val expectedValue = postValue * parms.gamma + feedback.reward

    // Computes the error by critic
    val delta = expectedValue - preValue

    // Teaches the critic by evidence
    val nc = critic.learn(s0Vect, DenseVector(expectedValue))

    // Computes the expected action preferences applying the critic error to previous decision */
    val pref = actor(s0Vect).output
    val expectedPref = pref.copy
    val action = feedback.action
    expectedPref(action to action) += parms.beta * delta

    // Teaches the actor by evidence
    val na = actor.learn(s0Vect, expectedPref)

    val nag = if (endEpisode) {
      new TDAgent(parms, nc.clearTraces, na.clearTraces)
    } else {
      new TDAgent(parms, nc, na)
    }
    nag
  }

}

/** Factory for [[TDAgent]] instances */
object TDAgent {

  /**
   * Creates a TDAgent with TD parameter,
   *  critic layer network configuration,
   *  actor layer network configuration and
   *  weights within a range.
   */
  def apply(
    parms: TDParms,
    criticLayers: Seq[Int],
    actorLayers: Seq[Int],
    wEpsilon: Double): TDAgent =
    new TDAgent(parms,
      TDNeuralNet(criticLayers, parms, wEpsilon),
      TDNeuralNet(actorLayers, parms, wEpsilon))

  /**
   * Creates a TDAgent with TD parameter,
   *  single hidden layer networks and
   *  weights within a range.
   */
  def apply(
    parms: TDParms,
    statusSize: Int,
    actionCount: Int,
    hiddenLayerSize: Int,
    wEpsilon: Double): TDAgent =

    apply(parms,
      Seq(statusSize, hiddenLayerSize, 1),
      Seq(statusSize, hiddenLayerSize, actionCount),
      wEpsilon)
}
