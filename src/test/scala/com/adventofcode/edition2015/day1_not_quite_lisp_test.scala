package com.adventofcode.edition2015
import org.scalatest._

class NotQuiteLispTest extends FlatSpec { 
  it should "calculate the floor number based on open and closed parens" in {
    val solver = new NotQuiteLisp

    assert(solver.findFloor("(())") == 0)
    assert(solver.findFloor("()()") == 0)

    assert(solver.findFloor("(((") == 3)
    assert(solver.findFloor("(()(()(") == 3)
    assert(solver.findFloor("))(((((") == 3)

    assert(solver.findFloor("())") == -1)
    assert(solver.findFloor("))(") == -1)

    assert(solver.findFloor(")))") == -3)
    assert(solver.findFloor(")())())") == -3)
  }

  it should "calculate the position of the character that causes Santa to first enter the basement" in {
    val solver = new NotQuiteLisp

    assert(solver.firstTimeBasement(")") == 1)
    assert(solver.firstTimeBasement("()())") == 5)
  }
}