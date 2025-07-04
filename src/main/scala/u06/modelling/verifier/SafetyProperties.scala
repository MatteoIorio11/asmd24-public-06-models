package scala.u06.modelling.verifier

import scala.u06.utils.MSet

object SafetyProperties:
  /**
   * Safety property
   */
  trait Safety[S]:
    def isSafe(s: S): Boolean

  trait MultiSetSafety[T] extends Safety[MSet[T]]

  /** Mutual Exclusion
   * Place bounded to one take.
   */
  case class MutualExclusion[T](private val currentState: T) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean = s(currentState) <= 1

  /** Readers and Writers Mutual Exclusion
   * It is not possible to have readers and writers at the same time.
   */
  case class RWMutualExclusion[T](private val positions: Map[T, Set[T]]) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean =
      val first = positions.keys.map(k => s(k)).sum
      val second = positions.values.flatten.map(w => s(w)).sum
      !((first > 0 && second > 0) && second >= 1)

  /** Bounded
   * A place has at most maxTokens tokens.
   */
  case class Bounded[T](private val state: T, private val maxTokens: Int) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean = s(state) <= maxTokens


  def bounded[T](state: T, maxTokens: Int): MultiSetSafety[T] = Bounded[T](state, maxTokens)
  def rwMutualExclusion[T](positions: Map[T, Set[T]]): MultiSetSafety[T] = RWMutualExclusion[T](positions)
  def mutualExclusion[T](state: T): MultiSetSafety[T] = MutualExclusion[T](state)
