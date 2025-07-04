package scala.u06.modelling

import scala.u06.modelling.SystemAnalysis.{Path, paths}

import scala.collection.immutable.{LazyList, Seq}

/**
 * <<TOOLING>>
 * The current API might be re-organised: can we generate/navigate all paths (even with loops) thanks to caching and
 * lazy evaluation?
 * Some proposed the non-determinism is an effect, and can hence be handled by a Monad: can this idea be used to
 * refactor our meta-metamodel support?
 */
object CachePaths:
  trait LRUCache[S]:
    def execute(state: S, depth: Int, function: (S, Int) => Seq[Path[S]],
                isComplete: Path[S] => Boolean): Seq[Path[S]]

  case class LRUCacheImpl[S]() extends LRUCache[S]:
    private val cache = scala.collection.mutable.HashMap[(S, Int), Seq[Path[S]]]()

    override def execute(state: S, depth: Int, function: (S, Int) => Seq[Path[S]],
                         isComplete: Path[S] => Boolean): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap(d => {
        if (cache contains (state, d)) then {
          // cache hit
          cache((state, d))
        }
        else
          val allPaths = function(state, d)
          cache.put((state, d), allPaths)
          allPaths
      }) filter isComplete
