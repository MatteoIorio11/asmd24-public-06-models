package scala.u06.examples

import u06.modelling.PetriNet

object PNReadersAndWrites:
  enum Place:
    case P1, P2, P3, P4, P5, P6, P7

  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  def readersAndWriters = PetriNet[Place](
    MSet(P1) ~~> MSet(P2), // P1 -> P2
    MSet(P2) ~~> MSet(P3), // P2 -> P3
    MSet(P2) ~~> MSet(P4), // P2 -> P4
    MSet(P3) ~~> MSet(P5), // P3 -> P5
    MSet(P4) ~~> MSet(P5), // P4 -> P5
    MSet(P5) ~~> MSet(P6), // P5 -> P6 [Lock]
    MSet(P5) ~~> MSet(P7), // P5 -> P7 [Lock]
    MSet(P6) ~~> MSet(P1), // P6 -> P1
    MSet(P7) ~~> MSet(P1),  // P7 -> P1
    MSet(P7) ~~> MSet(P5) // P7 -> P5

  ).toSystem

@main def mainReadersAndWriters =
  import PNReadersAndWrites.*
  println(readersAndWriters.paths(MSet(P1), 7).toList.mkString("\n"))
