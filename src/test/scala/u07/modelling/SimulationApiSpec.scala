package scala.u07.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.should
import u07.examples.StochasticChannel.State.{DONE, IDLE}
import u07.examples.StochasticChannel.{State, stocChannel}
import u07.modelling.CTMCSimulation.Trace

import java.util.Random
import scala.collection.immutable.{List, Seq}
import scala.language.postfixOps
import scala.u07.examples.SimulationApi.SimulationBufferImpl
import scala.u07.examples.SimulationApi.{SimulationFilter, SimulationOperation}

class SimulationApiSpec extends AnyFunSuite:

  private var simulations: Seq[Trace[State]] = (1 to 10).map(_ => stocChannel.newSimulationTrace(IDLE, Random()).take(10))
    .flatMap(trace => LazyList(trace))
    .toSeq

  test ("Simulation Filter should correctly filter all the input traces with a specific filter"):
    val predicateApis = new SimulationBufferImpl(simulations) with SimulationFilter[State]
    val allDone = predicateApis.applyFilter(trace => trace.exists(s => s.state == DONE))
    allDone.simulations.forall(trace => trace.exists(s => s.state == DONE)) should be (true)

  test("Simulation Filter should correctlu count all the input traces with a specific filter"):
    val predicateApis = new SimulationBufferImpl(simulations) with SimulationFilter[State]
    val countIdle = predicateApis.applyCount(trace => trace.exists(s => s.state == IDLE))
    countIdle == predicateApis.size

  test("Simulation Operation should correctly map the traces into newer trace"):
    val operationApi = new SimulationBufferImpl(simulations) with SimulationOperation[State]
