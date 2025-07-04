package scala.u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.shouldBe

import scala.u06.examples.PNReadersAndWrites
import scala.u06.examples.PNReadersAndWrites.{P1, P5, P6, P7, Place, readersAndWriters}
import scala.u06.modelling.SystemAnalysis.behaviourProperty
import scala.u06.modelling.verifier.BehaviourProperties.*
import scala.u06.modelling.verifier.BehaviourProperties.{Reachability, reachabilty}
import scala.u06.modelling.verifier.SafetyProperties.{Bounded, RWMutualExclusion, bounded, mutualExclusion, rwMutualExclusion}
import scala.u06.modelling.verifier.BehaviourProperties.*
import scala.u06.modelling.System.*
import scala.u06.examples.PNReadersAndWrites.*


class BehaviourPropertiesSpec extends AnyFunSuite:
  val initialMarking: MSet[Place] = MSet(P1, P1, P5)

  test("Mutual Exclusion: No Readers and writers at the same time"):
    readersAndWriters.
      safetyProperty(initialMarking, 100)(rwMutualExclusion(Map(P6 -> Set(P7)))) shouldBe true

  test("Mutual Exclusion: The resource should only be taken by one process"):
    readersAndWriters
      .safetyProperty(initialMarking, 100)(mutualExclusion(P5)) shouldBe true

  test("Bounded: We have always one shared resource"):
    readersAndWriters
      .safetyProperty(initialMarking, 100)(bounded(P5, 1)) shouldBe true

  test("Reachability: At some point it is possible to read"):
    readersAndWriters
      .behaviourProperty(initialMarking, 100)(reachabilty(P6)) shouldBe true

  test("Deadlock Freeness: The Readers and Writers petri net should be free from deadlocks"):
    readersAndWriters
      .behaviourProperty(initialMarking, 100)(deadlockFreeness(state => readersAndWriters.next(state))) shouldBe true


  test("Fairness: At some point it should be possible to do both operations (not at the same time)"):
    readersAndWriters
      .behaviourProperty(initialMarking, 100)(fairness(Set(P6, P7))) shouldBe true


  test("Reversibility: At every run it should be possible to start again from the initial state"):
    readersAndWriters
      .behaviourProperty(MSet(P1, P5), 100)(reversibility(P1)) shouldBe true
