package scala.u06.modelling.verifier

import u06.utils.MSet

object BehaviourProperties:
  trait Property[T]:
    def isValid(traces: Seq[T]): Boolean


  // Reachability:
  // definition: Whether a certain marking (state) can be reached from the initial marking.
  case class Reachability[T](private val state: T) extends Property[MSet[T]]:
    override def isValid(traces: Seq[MSet[T]]): Boolean = traces.exists(multiSet => multiSet(state) > 0)


  // Deadlock-freeness
  // Definition: Checks that the system never reaches a marking where no transition is enabled (no further progress possible).
  case class DeadlockFreeness[T](private val deadlocks: Set[T]) extends Property[MSet[T]]:
    override def isValid(traces: Seq[MSet[T]]): Boolean =
      deadlocks.map(state => traces.map(mset => mset(state)).sum).sum == 0

  // Fairness
  // Definition: Ensures that no transition is indefinitely postponed if it is enabled infinitely often.
  case class Fairness[T](private val allowedStates: Set[T]) extends Property[MSet[T]]:
    override def isValid(traces: Seq[MSet[T]]): Boolean =
      allowedStates.forall(state => traces.map(mset => mset(state)).sum > 0)


  object CheckBehaviourProperties:
    def reachabilty[T](state: T): Property[MSet[T]] = Reachability[T](state)
    def deadlockFreeness[T](deadlocks: Set[T]): Property[MSet[T]] = DeadlockFreeness[T](deadlocks)
    def fairness[T](allowedStates: Set[T]): Property[MSet[T]] = Fairness[T](allowedStates)
