package com.adventofcode.edition2015
import org.scalatest._
import scala.io.Source

class FireHazardTest extends FlatSpec {
  it should "calculate how many lights are lit" in {
    val solver = new FireHazard

    val filename = "day6_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      solver.applyInstruction(line)
    }

    assert(solver.litLights == 400410)
    assert(solver.totalBrightness == 15343601)
  }
}
