package u07.utils

import java.util.Random
import java.util.random.RandomGenerator

object Stochastics:

  /**
   * <<RANDOM-UNIT-TESTER>>
   * How do we unit-test with randomness? And how we test at all with randomness? Think about this in general. Try to create a
   * repeatable unit test for Statistics as in utils.StochasticSpec.
   */
  given RandomGenerator = new Random()

  // (p1,a1),...,(pn,an) --> (p1,a1),(p1+p2,a2),..,(p1+..+pn,an)
  def cumulative[A](l: List[(Double, A)]): List[(Double, A)] =
    l.tail.scanLeft(l.head):
      case ((r, _), (r2, a2)) => (r + r2, a2)

  // (p1,a1),...,(pn,an) --> ai, selected randomly and fairly
  def draw[A](cumulativeList: List[(Double,A)])(using rnd: RandomGenerator): A =
    val rndVal = rnd.nextDouble() * cumulativeList.last._1
    cumulativeList.collectFirst:
      case (r, a) if r >= rndVal => a
    .get

  // (p1,a1),...,(pn,an) --> {a1 -> P1%,...,an -> Pn%}
  def statistics[A](choices: Set[(Double,A)], size: Int)
                   (using rnd: RandomGenerator): Map[A, Int] =
    (1 to size).map(_ => draw(cumulative(choices.toList)))
                .groupBy(identity).view.mapValues(_.size).toMap

