package u07.utils

import org.mockito.Mockito
import org.mockito.Mockito.when
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import org.scalactic.Tolerance.convertNumericToPlusOrMinusWrapper
import org.scalatest.BeforeAndAfter

import java.util.Random
import java.util.random.RandomGenerator

/**
 * <<RANDOM-UNIT-TESTER>>
 * How do we unit-test with randomness? And how we test at all with randomness? Think about this in general. Try to create a
 * repeatable unit test for Statistics as in utils.StochasticSpec.
 */
class StochasticsSpec extends AnyFunSuite with BeforeAndAfter:
  val myMockRandom: RandomGenerator = Mockito.mock(classOf[RandomGenerator])
  val choices = Set( 1.0->"a", 2.0->"b", 3.0->"c")

  before:
    when(myMockRandom.nextDouble()).thenReturn(0.3)

  test("Choices should correctly give cumulative list"):
      Stochastics.cumulative(choices.toList) shouldBe
        List((1.0,"a"), (3.0,"b"), (6.0,"c"))

  test("Choices should correctly draw"):
    when(myMockRandom.nextDouble()).thenReturn(0.05, 0.3, 0.6)
    var map = Stochastics.statistics(choices, 10000)(using myMockRandom)
    map("a") shouldBe 1
    map("b") shouldBe 1
    map("c") shouldBe 9998

