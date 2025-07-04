package scala.u06.modelling.verifier

import scala.u06.utils.MSet

/** <<VERIFIER>>
 * Consider Readers & Writers Petri Net. Add a test to check that in no path long at most 100 states mutual exclusion fails (no more than 1 writer, and no readers and
 * writers together). Can you extract a small API for representing safety properties? What other properties can be extracted? How the boundness assumption can help?
 */
object BehaviourProperties:
  /**
   * Safety property of a petri net
   */
  trait Property[T]:
    def isValid(traces: Seq[T]): Boolean

  private trait MultiSetProperty[T] extends Property[MSet[T]]

  /** Reachability
   * Whether a certain marking (state) can be reached from the initial marking.
   */
  private case class Reachability[T](private val state: T) extends MultiSetProperty[T]:
    override def isValid(traces: Seq[MSet[T]]): Boolean = traces.exists(multiSet => multiSet(state) > 0)

  /** Deadlock-freeness
   * Definition: Checks that the system never reaches a marking where no transition is enabled (no further progress possible).
   */
  private case class DeadlockFreeness[T](private val deadlockCondition: T => Iterable[T]) extends Property[T]:
    override def isValid(traces: Seq[T]): Boolean =
      !(traces exists (s => deadlockCondition(s).isEmpty))

  /** Fairness
   * Definition: Ensures that no transition is indefinitely postponed if it is enabled infinitely often.
   */
  private case class Fairness[T](private val allowedStates: Set[T]) extends MultiSetProperty[T]:
    override def isValid(traces: Seq[MSet[T]]): Boolean =
      allowedStates.forall(state => traces.map(mset => mset(state)).sum > 0)

  /** Reversibility
   * Whether the system can always return to the initial marking, no matter what marking is reached
   */
  private case class Reversibility[T](private val initialState: T) extends MultiSetProperty[T]:
    override def isValid(traces: Seq[MSet[T]]): Boolean =
      (traces map (mset => mset(initialState))).sum > 1

  def reachabilty[T](state: T): Property[MSet[T]] = Reachability[T](state)
  def deadlockFreeness[T](deadlockCondition: T => Iterable[T]): Property[T] = DeadlockFreeness[T](deadlockCondition)
  def fairness[T](allowedStates: Set[T]): Property[MSet[T]] = Fairness[T](allowedStates)
  def reversibility[T](initialState: T): Property[MSet[T]] = Reversibility[T](initialState)
