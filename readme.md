
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
		epsilon = 0.1,
		lambda = 0.1,
		eta = 1.0,
		maxTrainingSample = 1)
	
	val agent = TDAgent(
		parms = parms,
		sigma = 1.0,
		statusSize = 10,
		actionCount = 4,
		10)


To have the explanations of parameters see ...



	
## Interacting the environment

The environment interaction consists of a cycle where the agent selects an action by the current status, the action is applied to the status to get the environment feedback and the feedback is used by current agent to train a new agent.
The feedback contains the new status of environment.
    

    val status : Status = ...
    val agent : TDAgent = ...
    
    val action = agent.action(status)
    val feedback = status(action)
    val (agent1, error) = agent.train(feedback)
    val Feedback(_, _, reward, status1) = feedback


## Saving agent

The agent may be saved into a set of csv files

	agent.write("myagent")

The method writes the files:

  - `myagent-parms.csv` with the parameter set
  - `myagent-critic-n.csv` with the number of critic layers
  - `myagent-critic-0.csv` with the weight of critic network for first layer
  - `myagent-critic-{i}.csv` with the weight of critic network for (i+1) layer
  - `myagent-critic-{n-1}.csv` with the weight of critic network for last layer
  - `myagent-actor-n.csv` with the number of critic layers
  - `myagent-actor-0.csv` with the weight of critic network for first layer
  - `myagent-actor-{i}.csv` with the weight of critic network for (i+1) layer
  - `myagent-actor-{n-1}.csv` with the weight of critic network for last layer


## Loading agent

*Not yet implemented ...*


# Akka actors implementation

The environment iteration is implemented by akka actors, which allow to run concurrent training while interacting the environment.

To use the akka implementation you need to create a environment actor:

    val initStatus: Status = ...
    val agent : TDAgent = ...

    val environmentActor = system.actorOf(EnvironmentActor.props(initStatus, agent))
    
Then you send an `Interact` message to the actor

	environmentActor ! Interact
	
The actor will reply with a `Step(feedback, error, agent)` message containing the feedback, the error and the new agent generated.


You may query the current agent by sending a `QueryAgent` message

	environmentActor ! QueryAgent
 
The actor will reply with a `CurrentAgent(agent)` message containing the current agent.
 
The akka implementation creates a trainer akka actor that trains in background mode the
critic neural network.

The `maxTrainingSample` of the `TDParms` sets the number of
recent samples used in training process.

If the `maxTrainingSample` is set to zero no batch trainer will be created.
