package scala.u07.examples

import u07.examples.StochasticChannel.State
import u07.modelling.CTMCSimulation.Trace

object TracesLogic:
  sealed trait TraceLogic[X]:
    /**
     * Apply a predicate to the stored values, return a new TraceLogic containing the filtered values.
     * @param predicate: Predicate to apply
     * @return A new TraceLogic containing only the values that pass the predicate.
     */
    def applyPredicate(predicate: X => Boolean): TraceLogic[X]

    /**
     * Count all the values that pass the predicate.
     * @param predicate: Predicate to apply in order to filter all the elements.
     * @return The total amount of values that pass the predicate.
     */
    def applyCount(predicate: X => Boolean): Int

    /**
     * Apply an input function and convert all the values of X into values of Y.
     * @param function: Function to apply
     * @tparam Y: Output type of the function
     * @return A new TraceLogic containing the new values after applying the function.
     */
    def applyFunction[Y](function: X => Y): TraceLogic[Y]

    /**
     * Apply the input operator to the values.
     * @param operator: Operator to apply.
     * @return The result after applying the operator.
     */
    def applyOperator(operator: (X, X) => X): X

    /**
     * Mean of all the values that are true to the predicate.
     * @param predicate: Predicate to apply in order to check how many value of a type we have.
     * @return The final mean.
     */
    def meanForValue(predicate: X => Boolean): Double

    /**
     * Get the copy of the values stored.
     * @return The copy of all the values stored
     */
    def collection: Seq[X]

  def apply[X](traces: Seq[X]): TraceLogic[X] = new TracesLogic(traces)

  class TracesLogic[X](private val traces: Seq[X]) extends TraceLogic[X]:
    private val buffer = List().appendedAll(traces)

    override def applyPredicate(predicate: X => Boolean): TracesLogic[X] =
      new TracesLogic(buffer.filter(trace => predicate(trace)))

    override def applyCount(predicate: X => Boolean): Int =
      buffer.count(trace => predicate(trace))

    override def applyFunction[Y](function: X => Y): TraceLogic[Y] =
      new TracesLogic(buffer.map(trace => function(trace)))

    override def applyOperator(operator: (X, X) => X): X =
      buffer.reduce((a, b) => operator.apply(a, b))

    override def meanForValue(predicate: X => Boolean): Double =
      val totalInState: Double = this.buffer.count(predicate)
      buffer.size match
        case 0 => 0
        case x => totalInState / x

    override def collection: Seq[X] = List() ++ buffer
