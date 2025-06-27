package u06.modelling

import u06.examples.PNReadersAndWrites

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

    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _)) filter complete

    /**
     * <<TOOLING>>
     * The current API might be re-organised: can we generate/navigate all paths (even with loops) thanks to caching and lazy evaluation?
     */
    def completePathsUpToDepthWithCache(s: S, depth:Int): Seq[Path[S]] =
      lazy val cache = scala.collection.mutable.HashMap[(S, Int), Seq[Path[S]]]()
      (1 to depth).to(LazyList) flatMap(d => {
        if (cache contains (s, d)) then {
          // cache hit
          cache((s, d))
        }
        else
          val allPaths = paths(s, d)
          cache.put((s, d), allPaths)
          allPaths
      }) filter complete

  /**
   * <<VERIFIER>> Partial implementation of the overall task, inside this code there is the logic for checking
   * if there is an intersection between all the input critical sections
   * @param sequences: all the paths explored
   * @tparam S: type of the state
   * @return true if the firstCriticalSection and the secondCriticalSection does not happen at the same time
   */
  def isSafe[S](sequences: Seq[Path[S]], criticalSections: Set[S]): Boolean =
    import scala.util.boundary, boundary.break
    val intervals = scala.collection.mutable.HashMap[S, Set[Int]]()
    for path <- sequences do
      for i <- path.indices do
        intervals.put(path(i), intervals.getOrElse(path(i), Set()) + i)
    boundary:
      for section <- criticalSections do
        val notSafe = intervals.filter(entry => criticalSections.contains(entry._1) && !entry._1.equals(section))
          .flatMap(entry => entry._2)
          .exists(time => intervals(section).contains(time))
        if notSafe then break(false)
      true
