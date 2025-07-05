package scala.u07.examples

import scala.u07.modelling.CTMCSimulation.Trace

/**
 * <<SIMULATOR>>
 * Take the communication channel CTMC example in StochasticChannelSimulation. Compute the average time at which
 * communication is done—across n runs. Compute the relative amount of time (0% to 100%) that the system is in fail state until
 * communication is done—across n runs. Extract an API for nicely performing similar checks.
 */
object SimulationApi:
  private type Simulations[S] = Seq[Trace[S]]
  // also very good with extension methods on Simulations!
  trait SimulationBuffer[S]:
    def simulations: Simulations[S]
    def size: Int
  trait SimulationOperation[S] extends SimulationBuffer[S]:
    def applyFunction[X](function: Trace[S] => Trace[X]): SimulationBuffer[X] =
      simulationBuffer(simulations map function)
    def applyTakeFirstN(n: Int): SimulationBuffer[S] =
      simulationBuffer(simulations take (n))
  trait SimulationFilter[S] extends SimulationBuffer[S]:
    def applyFilter(predicate: Trace[S] => Boolean): SimulationBuffer[S] =
      simulationBuffer(simulations filter predicate)
    def applyTakeUntil(predicate: Trace[S] => Boolean): SimulationBuffer[S] =
      simulationBuffer(simulations takeWhile predicate)
    def applyCount(predicate: Trace[S] => Boolean): Int = simulations count predicate
  trait SimulationStatistic[S] extends SimulationBuffer[S]:
    def meanForTrace(predicate: Trace[S] => Boolean): Double

  private case class SimulationProcessorImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S]
    with SimulationOperation[S]
    with SimulationFilter[S]
    with SimulationStatistic[S]:

    override def size: Int = simulations.size

    override def meanForTrace(predicate: Trace[S] => Boolean): Double = size match
      case 0 => 0
      case x =>
        applyCount(predicate) / x.toDouble


  case class SimulationBufferImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S]:
    override def size: Int = simulations.size


  def simulationBuffer[S](simulations: Simulations[S]): SimulationBuffer[S] = SimulationBufferImpl[S](simulations)
  def simulationProcessor[S](simulations: Simulations[S]): SimulationStatistic[S] = SimulationProcessorImpl[S](simulations)
