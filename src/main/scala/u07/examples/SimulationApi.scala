package scala.u07.examples

import u07.modelling.CTMCSimulation.Trace

object SimulationApi:
  private type Simulations[S] = Seq[Trace[S]]
  trait SimulationBuffer[S]:
    def simulations: Simulations[S]
    def size: Int
  trait SimulationOperation[S]:
    def applyFunction[X](function: Trace[S] => Trace[X]): SimulationBuffer[X]
    def applyTakeFirstN(n: Int): SimulationBuffer[S]
  trait SimulationFilter[S]:
    def applyFilter(predicate: Trace[S] => Boolean): SimulationBuffer[S]
    def applyTakeUntil(predicate: Trace[S] => Boolean): SimulationBuffer[S]
    def applyCount(predicate: Trace[S] => Boolean): Int
  trait SimulationStatistics[S]:
    def meanForTrace(predicate: Trace[S] => Boolean): Double

  case class SimulationImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S]
    with SimulationOperation[S]
    with SimulationFilter[S]
    with SimulationStatistics[S]:

    override def size: Int = simulations.size

    override def applyFunction[X](function: Trace[S] => Trace[X]): SimulationBuffer[X] =
      SimulationImpl(simulations map function)

    override def applyTakeFirstN(n: Int): SimulationBuffer[S] =
      SimulationImpl(simulations take (n))

    override def applyFilter(predicate: Trace[S] => Boolean): SimulationBuffer[S] =
      SimulationImpl(simulations filter predicate)

    override def applyCount(predicate: Trace[S] => Boolean): Int =
      simulations count predicate

    override def meanForTrace(predicate: Trace[S] => Boolean): Double = size match
      case 0 => 0
      case x =>
        applyCount(predicate) / x.toDouble

    override def applyTakeUntil(predicate: Trace[S] => Boolean): SimulationBuffer[S] =
      SimulationImpl(simulations takeWhile predicate)
