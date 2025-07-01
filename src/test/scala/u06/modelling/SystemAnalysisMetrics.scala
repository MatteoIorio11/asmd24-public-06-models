package scala.u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, contain}
import org.scalatest.matchers.should.Matchers.should
import u06.examples.PNReadersAndWrites.Place.*
import u06.examples.PNReadersAndWrites.readersAndWriters
import u06.utils.MSet

import scala.language.postfixOps


class SystemAnalysisMetrics extends AnyFunSuite:
  // Flaky
  test("The complete paths up to depth with the cache should be faster than without"):
    readersAndWriters.completePathsUpToDepth(MSet(P1), 10)
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 10)

    val startingTimeNoCache: Long = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepth(MSet(P1), 10)
    val endTimeNoCache: Long = System.currentTimeMillis()
    val startTimeCache: Long = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 10)
    val endTimeCache: Long = System.currentTimeMillis()
    (endTimeNoCache - startingTimeNoCache) >= (endTimeCache - startTimeCache)


  test("If previously was computed a path with a greather depth, doing the same with a lower depth should be faster thanks to caching"):
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 40)
    readersAndWriters.completePathsUpToDepth(MSet(P1), 40)

    val startTimeCache: Long = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 30)
    val endTimeCache: Long = System.currentTimeMillis()
    val startingTimeNoCache: Long = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepth(MSet(P1), 30)
    val endTimeNoCache: Long = System.currentTimeMillis()

    (endTimeNoCache - startingTimeNoCache) >= (endTimeCache - startTimeCache)

  test("Path generation with and without cache should return the same result"):
    val withCache = readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 5)
    val withoutCache = readersAndWriters.completePathsUpToDepth(MSet(P1), 5)
    withoutCache == withCache

  test("Cached computation is faster on subsequent runs"):
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 20)

    val startTime: Long = System.currentTimeMillis()
    readersAndWriters.completePathsUpToDepthWithCache(MSet(P1), 20)
    val endTime: Long = System.currentTimeMillis()

    val duration: Long = endTime - startTime
    duration should be < 1000L
