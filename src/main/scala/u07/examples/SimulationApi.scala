package scala.u07.examples

import scala.u07.examples.StochasticChannel.State.IDLE
import scala.u07.modelling.CTMCSimulation.{Event, Trace}
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
    def map[X](fun: Trace[S] => Trace[X]): SimulationBuffer[X] =
      simulationBuffer(simulations.map(fun(_)))
    def flatMap[X](fun: Trace[S] => SimulationBuffer[X]): SimulationBuffer[X] =
      simulationBuffer(simulations.flatMap(fun(_).simulations))

  trait SimulationOperation[S] extends SimulationBuffer[S]:
    def transformSimulations[X](function: Trace[S] => Trace[X]): SimulationOperation[X] =
      simulationOperation(simulations map function)
    def takeFirstNSimulations(n: Int): SimulationOperation[S] =
      simulationOperation(simulations take (n))
    override def map[X](fun: Trace[S] => Trace[X]): SimulationOperation[X] =
      simulationOperation(super.map(fun).simulations)
    override def flatMap[X](fun: Trace[S] => SimulationBuffer[X]): SimulationOperation[X] =
      simulationOperation(super.flatMap(fun).simulations)

  trait SimulationFilter[S] extends SimulationBuffer[S]:
    def filterSimulations(predicate: Trace[S] => Boolean): SimulationFilter[S] =
      simulationFilter(simulations filter predicate)
    def takeSimulationsUntil(predicate: Trace[S] => Boolean): SimulationFilter[S] =
      simulationFilter(simulations takeWhile predicate)
    def countSimulationsOf(predicate: Trace[S] => Boolean): Int = simulations count predicate
    override def map[X](fun: Trace[S] => Trace[X]): SimulationFilter[X] =
      simulationFilter(super.map(fun).simulations)
    override def flatMap[X](fun: Trace[S] => SimulationBuffer[X]): SimulationFilter[X] =
      simulationFilter(super.flatMap(fun).simulations)

  trait SimulationStatistic[S] extends SimulationBuffer[S]:
    def meanForTrace(predicate: Trace[S] => Boolean): Double

  trait SimulationProcessor[S] extends SimulationFilter[S] with SimulationOperation[S] with SimulationStatistic[S]:
    override def transformSimulations[X](function: Trace[S] => Trace[X]): SimulationProcessor[X] =
      simulationProcessor(super.transformSimulations(function).simulations)
    override def filterSimulations(predicate: Trace[S] => Boolean): SimulationProcessor[S] =
      simulationProcessor(super.filterSimulations(predicate).simulations)
    override def takeSimulationsUntil(predicate: Trace[S] => Boolean): SimulationProcessor[S] =
      simulationProcessor(super.takeSimulationsUntil(predicate).simulations)
    override def takeFirstNSimulations(n: Int): SimulationProcessor[S] =
      simulationProcessor(super.takeFirstNSimulations(n).simulations)
    override def map[X](fun: Trace[S] => Trace[X]): SimulationProcessor[X] =
        simulationProcessor(super.map(fun).simulations)
    override def flatMap[X](fun: Trace[S] => SimulationBuffer[X]): SimulationProcessor[X] =
      simulationProcessor(super.flatMap(fun).simulations)


  private case class SimulationProcessorImpl[S](override val simulations: Simulations[S]) extends SimulationProcessor[S] with SimulationStatistic[S]:
    override def size: Int = simulations.size

    override def meanForTrace(predicate: Trace[S] => Boolean): Double = size match
      case 0 => 0
      case x =>
        countSimulationsOf(predicate) / x.toDouble


  case class SimulationBufferImpl[S](override val simulations: Simulations[S]) extends SimulationBuffer[S]:
    override def size: Int = simulations.size

  def simulationBuffer[S](simulations: Simulations[S]): SimulationBuffer[S] =
    SimulationBufferImpl[S](simulations)
  def simulationFilter[S](simulations: Simulations[S]): SimulationFilter[S] =
    new SimulationBufferImpl[S](simulations) with SimulationFilter[S]
  def simulationOperation[S](simulations: Simulations[S]): SimulationOperation[S] =
    new SimulationBufferImpl[S](simulations) with SimulationOperation[S]
  def simulationProcessor[S](simulations: Simulations[S]): SimulationProcessor[S] = SimulationProcessorImpl[S](simulations)
