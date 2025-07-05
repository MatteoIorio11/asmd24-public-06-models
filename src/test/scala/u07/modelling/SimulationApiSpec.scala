package scala.u07.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.should

import scala.u07.examples.StochasticChannel.State.{DONE, FAIL, IDLE}
import scala.u07.examples.StochasticChannel.{State, stocChannel}
import scala.u07.modelling.CTMCSimulation.Trace
import java.util.Random
import scala.collection.immutable.{List, Seq}
import scala.language.postfixOps
import scala.u07.examples.SimulationApi.{SimulationBufferImpl, SimulationFilter, SimulationOperation, simulationBuffer}

/**
 * <<SIMULATOR>>
 * Take the communication channel CTMC example in StochasticChannelSimulation. Compute the average time at which
 * communication is done—across n runs. Compute the relative amount of time (0% to 100%) that the system is in fail state until
 * communication is done—across n runs. Extract an API for nicely performing similar checks.
 */
class SimulationApiSpec extends AnyFunSuite:

  private val simulations: Seq[Trace[State]] = (1 to 10).map(_ => stocChannel.newSimulationTrace(IDLE, Random()).take(40))
    .flatMap(trace => LazyList(trace))

  test ("Simulation Filter should correctly filter all the input traces with a specific filter"):
    val predicateApis = new SimulationBufferImpl[State](simulations) with SimulationFilter[State]
    val allDone = predicateApis.applyFilter(trace => trace.exists(s => s.state == DONE))
    allDone.simulations forall(trace => trace.exists(s => s.state == DONE)) should be (true)

  test("Simulation Filter should correctlu count all the input traces with a specific filter"):
    val predicateApis = new SimulationBufferImpl[State](simulations) with SimulationFilter[State]
    val countIdle = predicateApis.applyCount(trace => trace.exists(s => s.state == IDLE))
    countIdle == predicateApis.size

  test("Simulation Filter should correctly take all the traces until a specific state"):
    val predicateApis = new SimulationBufferImpl[State](simulations) with SimulationFilter[State]
    val withoutFail = predicateApis.applyTakeUntil(trace => trace.exists(s => s.state == FAIL))
    withoutFail.simulations forall(trace => trace.count(s => s.state == FAIL) == 0) should be (true)

  test("Simulation Operation should correctly map the traces into newer trace"):
    val operationApi = new SimulationBufferImpl[State](simulations) with SimulationOperation[State]
    val mappedTraces = operationApi.applyFunction(trace => LazyList(trace.last))
    mappedTraces.simulations forall(trace => trace.size == 1) should be (true)

  test("Simulation Operation should correctly take N traces"):
    val N = 1
    val operationApi = new SimulationBufferImpl[State](simulations) with SimulationOperation[State]
    val firstN = operationApi.applyTakeFirstN(N)
    firstN.size == N

