package u07.examples

import u07.modelling.CTMC

import java.util.Random
import scala.u07.examples.ExtractorBuilder

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
  def meanDone(trace: Seq[Event[State]]): Double =
    val totalDone: Double = ExtractorBuilder(trace)
      .applyCount(trace => trace.state == DONE)
    val totalStates: Double = ExtractorBuilder(trace).applyCount(tr => true)
    (totalDone / totalStates) * 100
//    tracesApi(trace, f=> trace.filter(seq => seq.last.state == DONE).map(seq => seq.last.time).sum / trace.size)

  def meanFailToDone(trace: Seq[Trace[State]]): Double =
    val totalFail = ExtractorBuilder(trace).applyCount(trace => trace.contains(FAIL))
    val failToDone = ExtractorBuilder(trace).applyCount(trace => trace.contains(FAIL) && trace.last.state == DONE)
    if totalFail > 0 then
      failToDone / totalFail
    else
      0

@main def mainStochasticChannel() =  // example run
  import StochasticChannel.*
  State.values.foreach(s => println(s"$s,${stocChannel.transitions(s)}"))
  (1 to 10).map(_ => stocChannel.newSimulationTrace(IDLE, Random()).take(10).toList)
    .foreach(trace => {
      println(trace.mkString("\n"))
      print(meanDone(trace))
    })
