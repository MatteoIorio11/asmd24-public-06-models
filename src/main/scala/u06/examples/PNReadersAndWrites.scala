package u06.examples

import u06.modelling.PetriNet


object PNReadersAndWrites:
  enum Place:
    case P1, P2, P3, P4, P5, P6, P7

  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  def readersAndWriters = PetriNet[Place](
    MSet(P1) ~~> MSet(P2), // t1
    MSet(P2) ~~> MSet(P3), // t2
    MSet(P2) ~~> MSet(P4), // t3
    MSet(P3) ~~> MSet(P5), // t4
    MSet(P3) ~~> MSet(P6), // t4
    MSet(P4) ~~> MSet(P7), // t5
    MSet(P5) ~~> MSet(P3), // t4 [lock]
    MSet(P5) ~~> MSet(P7), // t5 [Lock]
    MSet(P6) ~~> MSet(P1), // t6
    MSet(P6) ~~> MSet(P7) ^^^ MSet(P6), // t5
    MSet(P7) ~~> MSet(P1),  // t7
    MSet(P7) ~~> MSet(P5) // t7

  ).toSystem

@main def mainReadersAndWriters =
  import PNReadersAndWrites.*
  println(readersAndWriters.paths(MSet(P1), 7).toList.mkString("\n"))
