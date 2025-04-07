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

  def readersAndWritesWithPriority = PetriNet[Place](
    MSet(P1) ~~> MSet(P2),                  // t1: Process starts
    MSet(P2) ~~> MSet(P3),                  // t2: Process decides to read
    MSet(P2) ~~> MSet(P4),                  // t3: Process decides to write
    MSet(P3) ~~> MSet(P5),                  // t4: Start reading
    MSet(P3) ~~> MSet(P8),                  // NEW: Reader goes to waiting queue if can't read immediately
    MSet(P8) ~~> MSet(P5),                  // NEW: Reader from queue starts reading (priority)
    MSet(P5) ~~> MSet(P6),                  // t5: Finish reading
    MSet(P4) ~~> MSet(P7),                  // t6: Start writing (only if no readers waiting)
    MSet(P6) ~~> MSet(P1),                  // t7: Reader goes back to idle
    MSet(P7) ~~> MSet(P1),                  // t8: Writer goes back to idle
    MSet(P7) ~~> MSet(P8) ^^^ MSet(P7)      // NEW: Block writers if readers are waiting
  ).toSystem


@main def mainReadersAndWriters =
  import PNReadersAndWrites.*
  println(readersAndWriters.paths(MSet(P1), 7).toList.mkString("\n"))
  println(readersAndWritesWithPriority.paths(MSet(P1), 7).toList.mkString("\n"))
