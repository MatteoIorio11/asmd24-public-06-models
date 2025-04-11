package u07.examples

import u07.modelling.CTMC

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

// 1) Compute the average time at which communication is done—across n runs
// Compute the relative amount of time (0 % to 100 %) that the system is in fail state until communication is done—across n runs
  def meanDone(trace: Seq[Trace[State]]): Double =
    trace.filter(seq => seq.last.state == DONE).map(seq => seq.last.time).sum / trace.size

  def meanFailToDone(trace: Seq[Trace[State]]): Double =
    val totalFail = trace.count(seq => seq.contains(FAIL))
    val failToDone = trace.count(seq => seq.contains(FAIL) && seq.last.state == DONE)
    if totalFail > 0 then
      failToDone / totalFail
    else
      0

@main def mainStochasticChannel() =  // example run
  import StochasticChannel.*
  State.values.foreach(s => println(s"$s,${stocChannel.transitions(s)}"))
