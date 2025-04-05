package scala.u06.modelling

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import u06.modelling.SystemAnalysis

class TestReadersWriters extends AnyFunSuite:
  import u06.examples.PNReadersAndWrites.*
  import u06.utils.MSet

  test("When the computation starts from P1 and the length is 2, then the path should be P1 -> P2 -> P3"):
    val expected1 = List(MSet(P1), MSet(P2))
    readersAndWriters.paths(MSet(P1), 2).toSet should be:
      Set(expected1)

  test("When the computation starts from P2 and the length is 2, then the node can choose between P3 and P4"):
    val expected1 = List(MSet(P2), MSet(P3))
    val expected2 = List(MSet(P2), MSet(P4))
    readersAndWriters.paths(MSet(P2), 2).toSet should be:
      Set(expected1, expected2)

  test("When the computation starts from P5 and the length is 2, then the node can choose between P6 and P7"):
    val expected1 = List(MSet(P5), MSet(P3))
    val expected2 = List(MSet(P5), MSet(P7))
    readersAndWriters.paths(MSet(P5), 2).toSet should be:
      Set(expected1, expected2)

  test("When the computation starts from P6 and the length is 2, then the node can only go to P1"):
    val expected1 = List(MSet(P6), MSet(P1))
    readersAndWriters.paths(MSet(P6), 2).toSet should be:
      Set(expected1)

  test("When the computation starts from P4 and the length is 2, the the node can only go to P7"):
    val expected1 = List(MSet(P4), MSet(P7))
    readersAndWriters.paths(MSet(P4), 2).toSet should be:
      Set(expected1)

  test("When two critical sections happens at the same time, then the system is not safe and should return false"):
    val sequences: Seq[List[Place]] = Seq(
      List(P1, P2, P3, P5, P6), // t1 -> t2 -> t4 -> t6
      List(P1, P2, P3, P5, P7), // t1 -> t2 -> t4 -> t5
    )
    SystemAnalysis.isSafe(sequences, Set(P6, P7)) should be:
      false
