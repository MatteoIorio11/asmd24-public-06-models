package scala.u07.examples

import scala.u07.examples.StochasticMutualExclusion.Place.N
import scala.u07.modelling.SPN
import scala.u07.utils.MSet

import java.util.Random

object StochasticReadersWriters extends App:
  // Specification of my data-type for states
  enum Place:
    case p1, p2, p3, p4, p5, p6, p7

  val stochasticRW = SPN[Place](
    Trn(MSet(p1), m => 1.0,   MSet(p2),  MSet()), // t1
    Trn(MSet(p2), m => 200_000.0,   MSet(p3),  MSet()), // t2
    Trn(MSet(p2), m => 100_000.0,   MSet(p4),  MSet()), // t3
    Trn(MSet(p3, p5), m => 100_000.0,   MSet(p6, p5),  MSet()), // t4
    Trn(MSet(p4, p5), m => 100_000.0,   MSet(p7),  MSet(p6)), // t5
    Trn(MSet(p6), m => m(p6) * 0.1,   MSet(p1),  MSet()), // t6
    Trn(MSet(p7), m => 0.2,   MSet(p1, p5),  MSet()), // t7
  )

  export Place.*
  export scala.u07.modelling.CTMCSimulation.*
  export scala.u07.modelling.SPN.*

  println:
    toCTMC(stochasticRW).newSimulationTrace(MSet(p1, p5), new Random)
      .take(20)
      .toList.mkString("\n")
