package scala.u06.modelling.verifier

import u06.utils.MSet

object SafetyProperties:
  trait Safety[S]:
    def isSafe(s: S): Boolean

  trait MultiSetSafety[T] extends Safety[MSet[T]]

  case class MutualExclusion[T](currentState: T) extends MultiSetSafety[T]:
    override def isSafe(s: MSet[T]): Boolean = s(currentState) <= 1

