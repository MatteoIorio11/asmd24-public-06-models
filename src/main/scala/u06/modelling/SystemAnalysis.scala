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

    // same depth and same start and end of queue, same path
    def explore(start: S, depth: Int):Seq[Path[S]] =
      val deque = scala.collection.mutable.ArrayDeque[(Int, S, S)]()
      val cache = scala.collection.mutable.HashMap[(Int, S, S), Seq[Path[S]]]()
      Seq()



    // complete paths with length '<= depth' (could be optimised)
    def completePathsUpToDepth(s: S, depth:Int): Seq[Path[S]] =
      (1 to depth).to(LazyList) flatMap (paths(s, _)) filter (complete(_))

  /**
   * Check if the firstCriticalSection and the secondCriticalSection does not happen at the same time
   * @param sequences: all the paths explored
   * @param firstCriticalSection: first critical state
   * @param secondCriticalSection: second critical state
   * @tparam S: type of the state
   * @return true if the firstCriticalSection and the secondCriticalSection does not happen at the same time
   */
  def isSafe[S](sequences: Seq[Path[S]], firstCriticalSection: S, secondCriticalSection: S): Boolean =
    val readers = scala.collection.mutable.TreeSet[Int]()
    val writers = scala.collection.mutable.TreeSet[Int]()
    for path <- sequences do
      for i <- path.indices do
        if path(i) == firstCriticalSection then
          readers.add(i)
        else if path(i) == secondCriticalSection then
          writers.add(i)
    !readers.exists(i => writers.contains(i))
