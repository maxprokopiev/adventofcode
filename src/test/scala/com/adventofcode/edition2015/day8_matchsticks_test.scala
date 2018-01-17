package com.adventofcode.edition2015

import org.scalatest._
import scala.io.Source

class MatchsticksTest extends FlatSpec {
  it should "calculate a number of characters in the in-memory string itself" in {
    val solver = new Matchsticks

    assert(solver.inMemoryStringSizeFor("""z\xc\x03n\"\\h""") == 9)
  }

  it should "calculate the number of characters of code for string literals minus the number of characters in memory for the values of the strings in total for the entire file" in {
    val solver = new Matchsticks

    var ofCode = 0
    var inMemory = 0

    val filename = "day8_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      ofCode += line.size
      inMemory += solver.inMemoryStringSizeFor(line) - 2
    }    
    assert((ofCode - inMemory) == 1371)
  }

  it should "encode string properly" in {
    val solver = new Matchsticks

    assert(solver.encodeString("\"\"") == "\\\"\\\"")
  }

  it should "calculate the difference between original and encoded strings" in {
    val solver = new Matchsticks

    var ofOriginal = 0
    var ofEncoded = 0

    val filename = "day8_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      ofOriginal += line.size
      ofEncoded += solver.encodeString(line).size + 2
    }        
    assert((ofEncoded - ofOriginal) == 2117)
  }
}