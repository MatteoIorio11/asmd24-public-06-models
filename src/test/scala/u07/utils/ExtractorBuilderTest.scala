package scala.u07.utils

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import u07.examples.StochasticChannel
import u07.examples.StochasticChannel.State.{FAIL, IDLE, SEND}
import u07.examples.StochasticChannel.stocChannel
import u07.modelling.SPN.toCTMC
import u07.utils.MSet

import java.util.Random
import scala.swing.Action
import scala.u07.examples.ExtractorBuilder
import scala.u07.examples.StochasticReadersWriters.Place.{p1, p5}
import scala.u07.examples.StochasticReadersWriters.stochasticRW

class ExtractorBuilderTest extends AnyFunSuite:
  val sequence: Seq[MSet[StochasticChannel.State]] = Seq(
    MSet(IDLE, SEND, FAIL),
    MSet(IDLE, SEND, SEND, FAIL),
    MSet(IDLE, SEND, SEND),
    MSet(SEND, SEND, IDLE)
  )
  test("ExtractorBuilder should correctly filter a set o traces"):
    ExtractorBuilder(sequence).applyPredicate(s => s.asList.contains(FAIL)).collection.size shouldBe 2
    ExtractorBuilder(sequence).applyPredicate(s => s.asList.contains(IDLE)).collection.size shouldBe 4
    ExtractorBuilder(sequence).applyPredicate(s => s.asList.contains(SEND)).collection.size shouldBe 4

  test("ExtractorBuilder should correctly count a set of traces"):
    ExtractorBuilder(sequence).applyCount(s => s.asList.contains(FAIL)) shouldBe 2
    ExtractorBuilder(sequence).applyCount(s => s.asList.contains(IDLE)) shouldBe 4
    ExtractorBuilder(sequence).applyCount(s => s.asList.contains(SEND)) shouldBe 4

  test("ExtractorBuilder should correctly apply a function and operator to a set of traces"):
    ExtractorBuilder(sequence).applyFunctionAndOperator(s => s.asList.size, (i1, i2) => i1 + i2) shouldBe 13

  test("")
