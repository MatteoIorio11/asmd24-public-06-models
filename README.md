# System modelling, a Programming Language approach: non-determinism, unbounded size, Petri Nets
## Task lab06
Here are listed the task that I have made for the lab06
### TOOLING
Task: 

The current API might be re-organised: can we generate/navigate all paths (even with loops) thanks to caching and lazy evaluation?
Some proposed the non-determinism is an effect, and can hence be handled by a Monad: can this idea be used to refactor our meta-metamodel support?

Here you can find the code:
* SystemAnalysis.scala

## Task lab07
### SIMULATOR
Task:

Take the communication channel CTMC example in StochasticChannelSimulation. Compute the average time at which
communication is done—across n runs. Compute the relative amount of time (0% to 100%) that the system is in fail state until
communication is done—across n runs. Extract an API for nicely performing similar checks.

The code can be found here:
* StochasticChannel.scala
* SimulationApi.scala

## RANDOM-UNIT-TESTER
Task:

How do we unit-test with randomness? And how we test at all with randomness? Think about this in general. Try to create a
repeatable unit test for Statistics as in utils.StochasticSpec.

The code can be found here:
* Stochastics.scala
* StochasticsSpec.scala

