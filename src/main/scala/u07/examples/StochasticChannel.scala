package u07.examples

import u07.modelling.CTMC

import java.util.Random
import scala.u07.examples.TracesLogic

object StochasticChannel:
  enum State:
    case IDLE, SEND, DONE, FAIL;

  export State.*
  export u07.modelling.CTMCSimulation.*

  def stocChannel: CTMC[State] = CTMC.ofTransitions(
    Transition(IDLE,1.0 --> SEND),
    Transition(SEND,100000.0 --> SEND),
    Transition(SEND,200000.0 --> DONE),
    Transition(SEND,100000.0 --> FAIL),
    Transition(FAIL,100000.0 --> IDLE),
    Transition(DONE,1.0 --> DONE)
  )

  def tracesApi[X](trace: Seq[Trace[State]], function: Seq[Trace[State]] => X): X =
    function(trace)

  /**
   * <<SIMULATOR>>
   * Take the communication channel CTMC example in StochasticChannelSimulation. Compute the average time at which
   * communication is done—across n runs. Compute the relative amount of time (0% to 100%) that the system is in fail state until
   * communication is done—across n runs. Extract an API for nicely performing similar checks.
   */
  def meanDone(traces: Seq[Trace[State]]): Double =
    traces.size match
      case 0 => 0
      case x: Int =>
        var meanOfAllDone: Double = 0
          for (trace <- traces)
            meanOfAllDone += TracesLogic(trace).meanForValue(t => t.state == DONE)
        (meanOfAllDone / x) * 100

  def meanFailToDone(traces: Seq[Trace[State]]): Double =
    traces.size match
      case 0 => 0
      case totalSize =>
        var failToDone: Double = 0
        for (trace <- traces)
          failToDone = failToDone + (if (trace.count(t => t.state == FAIL) > 0 && trace.last.state == DONE) 1 else 0)
        (failToDone / totalSize) * 100

@main def mainStochasticChannel(): Unit =  // example run
  import StochasticChannel.*
  var traces:Seq[Trace[State]] = List()
  State.values.foreach(s => println(s"$s,${stocChannel.transitions(s)}"))
  (1 to 10).map(_ => stocChannel.newSimulationTrace(IDLE, Random()).take(10))
    .foreach(trace => {
      val lazyTrace = LazyList().appendedAll(trace)
      traces = traces :+ trace
      println(trace.mkString("\n"))
    })
  println("Mean of Done: " + meanDone(traces) + "%")
  println("Mean of Fail to Done: " + meanFailToDone(traces) + "%")
