package scala.u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe
import u06.examples.PNReadersAndWrites
import u06.examples.PNReadersAndWrites.{P1, P5, P6, P7, Place, readersAndWriters}
import u06.utils.MSet

import scala.u06.modelling.verifier.SafetyProperties.RWMutualExclusion

class BehaviourPropertiesSpec extends AnyFunSuite:
  val initialMarking: MSet[Place] = MSet(P1, P5)

  test("Reachability property"):
    val e = readersAndWriters.safetyProperty(initialMarking, 100)(RWMutualExclusion[Place](Set(P6), Set(P7)))
    e shouldBe (true)

