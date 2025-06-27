package scala.u06.modelling

import u06.modelling.SystemAnalysis.{Path, paths}

import scala.collection.immutable.{LazyList, Seq}

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
