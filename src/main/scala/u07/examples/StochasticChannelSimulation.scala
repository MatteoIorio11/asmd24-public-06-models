package scala.u07.examples

import scala.u07.utils.Time
import java.util.Random
import scala.u07.examples.StochasticChannel.*

@main def mainStochasticChannelSimulation =
  Time.timed:
    println:
      stocChannel.newSimulationTrace(IDLE, new Random)
        .take(10)
        .toList
        .mkString("\n")
