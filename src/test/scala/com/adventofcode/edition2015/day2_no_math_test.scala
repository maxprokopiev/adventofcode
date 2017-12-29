package com.adventofcode.edition2015
import org.scalatest._
import scala.io.Source

class NoMathTest extends FlatSpec {
  it should "calculate total square feet of wrapping paper to order" in {
    val solver = new NoMath

    assert(solver.totalSquare("2x3x4") == 58)
    assert(solver.totalSquare("1x1x10") == 43)

    var result = 0
    val filename = "day2_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      result += solver.totalSquare(line)
    }

    assert(result == 1586300)
  }

  it should "calculate total feet of ribbon for wrapping a box" in {
    val solver = new NoMath

    assert(solver.totalFeetOfRibbon("2x3x4") == 34)
    assert(solver.totalFeetOfRibbon("1x1x10") == 14)

    var result = 0
    val filename = "day2_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      result += solver.totalFeetOfRibbon(line)
    }

    assert(result == 3737498)
  }
}