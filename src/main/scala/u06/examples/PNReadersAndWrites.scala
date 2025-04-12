package u06.examples

import u06.modelling.PetriNet


object PNReadersAndWrites:
  enum Place:
    case P1, P2, P3, P4, P5, P6, P7, P8

  export Place.*
  export u06.modelling.PetriNet.*
  export u06.modelling.SystemAnalysis.*
  export u06.utils.MSet

  def readersAndWriters = PetriNet[Place](
    MSet(P1) ~~> MSet(P2), // t1
    MSet(P2) ~~> MSet(P3), // t2
    MSet(P2) ~~> MSet(P4), // t3
    MSet(P3, P5) ~~> MSet(P5, P6), // t4
    MSet(P4, P5) ~~> MSet(P7) ^^^ MSet(P6), // t5
    MSet(P6) ~~> MSet(P1), // t6
    MSet(P7) ~~> MSet(P1, P5),  // t7
  ).toSystem

  def readersAndWritesWithPriority = PetriNet[Place](
    MSet(P1) ~~> MSet(P2),                   // t1: request
    MSet(P2) ~~> MSet(P3),                   // t2: reader request
    MSet(P2) ~~> MSet(P4),                   // t3: writer request
    MSet(P3, P5) ~~> MSet(P5, P6),           // t4: reader enter
    MSet(P4, P5)  ~~> MSet(P7) ^^^ MSet(P3),  // t5: writer enter (guarded by "no readers")
    MSet(P6) ~~> MSet(P1),                   // t6: reader done
    MSet(P7) ~~> MSet(P1, P5),               // t7: writer done
  ).toSystem


@main def mainReadersAndWriters =
  import PNReadersAndWrites.*
//  println(readersAndWriters.paths(MSet(P1, P5), 7).toList.mkString("\n"))
  println(readersAndWritesWithPriority.paths(MSet(P3, P4, P5), 10).toList.mkString("\n"))
  // List({P5|P1|P1}, {P2|P5|P1}, {P5|P1|P3}, {P2|P5|P3}, {P2|P5|P6}, {P2|P5|P1}, {P2|P2|P5}, {P2|P5|P4}, {P5|P4|P3}, {P5|P6|P4})
