package scala.u06.modelling

import scala.u06.examples.PNReadersAndWrites
import scala.u06.utils.MSet

import scala.u06.modelling.CachePaths.LRUCacheImpl
import scala.u06.modelling.verifier.BehaviourProperties.*
import scala.u06.modelling.verifier.SafetyProperties.Safety

// Basical analysis helpers
object SystemAnalysis:

  type Path[S] = List[S]

  extension [S](system: System[S])

    def normalForm(s: S): Boolean = system.next(s).isEmpty

    def complete(p: Path[S]): Boolean = normalForm(p.last)

    // paths of exactly length `depth`
    def paths(s: S, depth: Int): Seq[Path[S]] = depth match
      case 0 => LazyList()
      case 1 => LazyList(List(s))
      case _ =>
        for
          path <- paths(s, depth - 1)
          next <- system.next(path.last)
        yield path :+ next

    def bfs(s: S, depth: Int): Set[S] =
      var seen = Set[S]()
      var queue = scala.collection.mutable.ArrayDeque(s)
      var currentDepth = 0
      while queue.nonEmpty && currentDepth < depth do
        val nextStates = for
          state <- queue
          others <- system.next(state)
          if !seen.contains(others)
        yield others
        seen ++= nextStates
        queue.appendAll(nextStates)
        currentDepth += 1
      seen


    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _)) filter complete


    /**
     * Lazily generates all possible traces (paths) from a given starting state `s`
     * through the state-transition system defined by `system.next`.
     *
     * This method performs a lazy depth-first traversal, avoiding cycles by
     * tracking visited states in a `seen` set.
     *
     * @param s The starting state from which to begin the trace generation.
     * @return A `LazyList` of states representing all reachable paths from `s`,
     *         lazily evaluated and avoiding revisiting already explored states.
     */
    private def generateTraces(s: S): LazyList[S] =
      def explore(seen: Set[S], current: S): LazyList[S] =
        val otherTraces =
          for
            next <- system.next(current).diff(seen).to(LazyList)
            traces <- explore(seen + current, next)
          yield (traces)
        LazyList(current) #::: otherTraces
      explore(Set(), s)

    /**
     * <<TOOLING>>
     * The current API might be re-organised: can we generate/navigate all paths (even with loops) thanks to caching and lazy evaluation?
     */
    def completePathsUpToDepthWithCache(s: S, depth: Int): Seq[Path[S]] =
      lazy val lru = LRUCacheImpl[S]()
      lru.execute(s, depth, paths, complete)


    def safetyProperty(state: S, depth: Int)(safety: Safety[S]): Boolean =
      bfs(state, depth) forall (path => safety.isSafe(path))

    def behaviourProperty(state: S, depth: Int)(property: Property[S]): Boolean =
      property.isValid(generateTraces(state).take(depth))
