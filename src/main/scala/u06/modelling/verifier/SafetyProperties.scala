package scala.u06.modelling.verifier

import u06.utils.MSet

object SafetyProperties:
  trait Safety[S]:
    def isSafe(s: S): Boolean

  trait MultiSetSafety[T] extends Safety[MSet[T]]

  //Mutual exclusion bounded to one toke.
  case class MutualExclusion[T](private val currentState: T) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean = s(currentState) <= 1

  //It is not possible to have readers and writers at the same time.
  case class RWMutualExclusion[T](private val readers: Set[T], writers: Set[T]) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean =
      val readersCount = readers.map(r => s(r)).sum
      val writersCount = writers.map(w => s(w)).sum
      !((readersCount > 0 && writersCount > 0) && writersCount >= 1)

  //A place has at most maxTokens tokens.
  case class Bounded[T](private val state: T, private val maxTokens: Int) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean = s(state) <= maxTokens
