package scala.u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, contain}
import org.scalatest.matchers.should.Matchers.should
import u06.examples.PNReadersAndWrites.Place.*
import u06.examples.PNReadersAndWrites.readersAndWriters
import u06.utils.MSet

import scala.language.postfixOps


class SystemAnalysisMetrics extends AnyFunSuite:
  test("The complete paths up to depth with the cache should be faster than without"):
    readersAndWriters.completePathsUpToDepth(MSet(P1), 7)
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 7)

    val startingTimeNoCache = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepth(MSet(P1), 7)
    val endTimeNoCache = System.currentTimeMillis()
    val startTimeCache = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 7)
    val endTimeCache = System.currentTimeMillis()
    (endTimeNoCache - startingTimeNoCache) should be >= (endTimeCache - startTimeCache)


  test("If previously was computed a path with a greather depth, doing the same with a lower depth should be faster thanks to caching"):
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 40)
    readersAndWriters.completePathsUpToDepth(MSet(P1), 40)

    val startTimeCache = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 15)
    val endTimeCache = System.currentTimeMillis()
    val startingTimeNoCache = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepth(MSet(P1), 15)
    val endTimeNoCache = System.currentTimeMillis()

    (endTimeNoCache - startingTimeNoCache) should be >= (endTimeCache - startTimeCache)
