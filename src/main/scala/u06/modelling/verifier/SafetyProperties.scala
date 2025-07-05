package scala.u06.modelling.verifier

import scala.u06.utils.MSet

/** <<VERIFIER>>
 * Consider Readers & Writers Petri Net. Add a test to check that in no path long at most 100 states mutual exclusion fails (no more than 1 writer, and no readers and
 * writers together). Can you extract a small API for representing safety properties? What other properties can be extracted? How the boundness assumption can help?
 */
object SafetyProperties:
  /**
   * Safety property
   */
  sealed trait SafetyCheck[S]:
    def isSafe(s: S): Boolean

  private sealed trait MultiSetSafety[T] extends SafetyCheck[MSet[T]]

  /** Readers and Writers Mutual Exclusion
   * It is not possible to have readers and writers at the same time.
   */
  private case class MutualExclusion[T](private val regions: Seq[MSet[T]]) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean =
      val map = s.asMap
      !(regions map(mset => mset.asMap.keys) exists(keys => keys.forall(k => map.getOrElse(k, 0) > 0)))

  /** Bounded
   * A place has at most maxTokens tokens.
   */
  private case class Bounded[T](private val state: T, private val maxTokens: Int) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean = s(state) <= maxTokens


  def bounded[T](state: T, maxTokens: Int): SafetyCheck[MSet[T]] = Bounded[T](state, maxTokens)
  def mutualExclusion[T](regions: Seq[MSet[T]]): SafetyCheck[MSet[T]] = MutualExclusion[T](regions)
