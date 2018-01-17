package com.adventofcode.edition2015

import org.scalatest._
import scala.io.Source

class MatchsticksTest extends FlatSpec {
  it should "calculate a number of characters in the in-memory string itself" in {
    val solver = new Matchsticks

    assert(solver.inMemoryStringSizeFor("""z\xc\x03n\"\\h""") == 9)
  }
}