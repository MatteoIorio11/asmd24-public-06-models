package scala.u07.examples

import u07.modelling.CTMCSimulation.Trace

object SimulationApi:
  private type Simulations[S] = Seq[Trace[S]]
  trait SimulationBuffer[S]:
    def simulations: Simulations[S]
  trait SimulationOperations[S]:
    def applyFunction[X](function: Trace[S] => Trace[X]): SimulationBuffer[X]
    def applyOperator[X](operator: (Trace[S], Trace[S] => Trace[X])): SimulationBuffer[X]
    def takeFirstN(n: Int): SimulationBuffer[S]

  case class SimulationImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S] with SimulationOperations[S]:


