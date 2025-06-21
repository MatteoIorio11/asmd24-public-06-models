package scala.u07.examples

import u07.examples.StochasticChannel.State
import u07.modelling.CTMCSimulation.Trace

class TracesLogic[X](private val traces: Seq[X]):
  private var buffer = List().appendedAll(traces)

  def applyPredicate(predicate: X => Boolean): TracesLogic[X] =
    buffer = buffer.filter(trace => predicate(trace))
    this

  def applyCount(predicate: X => Boolean): Int =
    buffer.count(trace => predicate(trace))

  def applyFunctionAndOperator[Y](function: X => Y, operator: (Y, Y) => Y): Y =
    buffer.map(trace => function(trace)).reduce((i1, i2) => operator(i1, i2))

  def meanForState(predicate: X => Boolean): Double =
    val totalInState: Double = this.buffer.count(predicate)
    buffer.size match
      case 0 => 0
      case x => totalInState / x

  def collection: Seq[X] = buffer
