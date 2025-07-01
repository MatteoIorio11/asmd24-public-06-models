package scala.u07.examples

import u07.modelling.CTMCSimulation.Trace

object SimulationApi:
  private type Simulations[S] = Seq[Trace[S]]
  // also very good with extension methods on Simulations!
  trait SimulationBuffer[S]:
    def simulations: Simulations[S]
    def size: Int
  trait SimulationOperation[S] extends SimulationBuffer[S]:
    def applyFunction[X](function: Trace[S] => Trace[X]): SimulationBuffer[X] =
      SimulationImpl(simulations map function)
    def applyTakeFirstN(n: Int): SimulationBuffer[S] =
      SimulationImpl(simulations take (n))
  trait SimulationFilter[S] extends SimulationBuffer[S]:
    def applyFilter(predicate: Trace[S] => Boolean): SimulationBuffer[S] =
      SimulationImpl(simulations filter predicate)
    def applyTakeUntil(predicate: Trace[S] => Boolean): SimulationBuffer[S] =
      SimulationImpl(simulations takeWhile predicate)
    def applyCount(predicate: Trace[S] => Boolean): Int = simulations count predicate
  trait SimulationStatistics[S] extends SimulationBuffer[S]:
    def meanForTrace(predicate: Trace[S] => Boolean): Double

  case class SimulationImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S]
    with SimulationOperation[S]
    with SimulationFilter[S]
    with SimulationStatistics[S]:

    override def size: Int = simulations.size

    override def meanForTrace(predicate: Trace[S] => Boolean): Double = size match
      case 0 => 0
      case x =>
        applyCount(predicate) / x.toDouble


  case class SimulationBufferImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S]:
    override def size: Int = simulations.size
