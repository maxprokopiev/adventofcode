package com.adventofcode.edition2015
import org.scalatest._
import scala.io.Source

class AssemblyTest extends FlatSpec {
  it should "evaluate instructions correctly" in {
    val solver = new Assembly

    solver.applyInstruction("123 -> x")
    solver.applyInstruction("456 -> y")
    solver.applyInstruction("x AND y -> d")
    solver.applyInstruction("x OR y -> e")
    solver.applyInstruction("x LSHIFT 2 -> f")
    solver.applyInstruction("y RSHIFT 2 -> g")
    solver.applyInstruction("NOT x -> h")
    solver.applyInstruction("NOT y -> i")

    assert(solver.signalOn("d") == 72)
    assert(solver.signalOn("e") == 507)
    assert(solver.signalOn("f") == 492)
    assert(solver.signalOn("g") == 114)
    assert(solver.signalOn("h") == 65412)
    assert(solver.signalOn("i") == 65079)
    assert(solver.signalOn("x") == 123)
    assert(solver.signalOn("y") == 456)
  }

  it should "apply instructions correctly reading from a file" in {
    val solver = new Assembly

    val filename = "day7_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      solver.applyInstruction(line)
    }  

    assert(solver.signalOn("a") == 16076)
  }
}