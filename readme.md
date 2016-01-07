
# ACTD

ACTD is a library that implements the Temporal Difference reinforcement learning algorithm based on Actor/Critic method with Artificial Neural Network.


# Getting Started


## Implementing Status trait

The Status trait models the environment task as a MDP (Markov Decision Process).

You have to implement three method of the trait:

	def toDenseVector: DenseVector[Double]

The returned vector contains the values for each feature describing the status.


	def finalStatus: Boolean

In case of episodic task the function should return true if this is a final state. 
  

	def apply(action: Action): Feedback

This is the core method of the environment task implementing the MDP rules.
It applies the selected action computing the next status and the reward.
The feedback is SARS format (Status, Action, Reward, Status):
the current status, the applied action, the reward for the transition and the next status.

If the current status is the end episode then the next status should be the initial status of next episode and the reward should be zero.


### Example

	object MyStatus {
		val S0 = new Status() {
			val toDenseVector = DenseVector(1.0, 0.0)

			def apply(action: Action) = if (action == 0) {
				Feedback(this, action, -1.0, S1)
			} else {
				Feedback(this, action, -1.0, S0)
			}
		}

		val S1 = new Status() {
			val toDenseVector = DenseVector(0.0, 1.0)

			def apply(action: Action) = if (action == 0) {
				Feedback(this, action, 1.0, S2)
			} else {
				Feedback(this, action, -1.0, S1)
			}
		}

		val S2 = new Status() {
			val toDenseVector = DenseVector(0.0, 0.0)

			val finalStatus = true

			def apply(action: Action) = Feedback(this, action, 0.0, S0)
		}
	}
	

## Creating Agent

The Agent is the intelligent component of the system.

It can be created with 

	val parms = TDParms(
		alpha = 10e-3,
		beta = 1.0,
		gamma = 0.99,
		lambda = 0.1,
		eta = 1.0)
	
	val agent = TDAgent(
		parms = parms,
		sigma = 1.0,
		statusSize = 10,
		actionCount = 4,
		10)


To have the explanations of parameters see ...


## Creating the initial environment

The environment models the interaction between the environment itself and the Agent.

It is created with the initial status and the initial agent

	val initStatus: Status = ...
	
	val env = Environment(
		status = S0,
		agent = agent)
	
	
## Interacting with the environment

You can run the interaction calling the `stepOver` method

    val (env0, env1, Feedback(s0, a, r, s1)) = env.stepOver

It results the starting environment itself, the next environment with the next status and the updated agent and the feedback of interaction.

The feedback consists of the initial status, the performed action, the given reward and the final status.

Another way to run the interaction is to convert the initial environment to a iterator
and use the iterator functions.

    val iter = env.iterator

    // Gets next interaction
    val next = iter.next
    
    // Gets next 10 steps
    val next10 = iter.take(10)
    
    // Gets next episode
    val episode = iter.takeWhile{ case (_, env1, _) => !env1.status.finalStatus}
