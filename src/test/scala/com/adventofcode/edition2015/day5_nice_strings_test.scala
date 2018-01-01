package com.adventofcode.edition2015
import org.scalatest._
import scala.io.Source

class NiceStringsTest extends FlatSpec {
  it should "calculate if a given string is nice" in {
    val solver = new NiceStrings

    assert(solver.isNice("ugknbfddgicrmopn") == true)
    assert(solver.isNice("aaa") == true)

    assert(solver.isNice("jchzalrnumimnmhp") == false)
    assert(solver.isNice("haegwjzuvuyypxyu") == false)
    assert(solver.isNice("dvszwmarrgswjxmb") == false)

    var result = 0
    val filename = "day5_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      if (solver.isNice(line)) {
        result += 1 
      }
    }

    assert(result == 258)
  }

  it should "calculate if a given string is _really_ nice" in {
    val solver = new NiceStrings

    assert(solver.isReallyNice("qjhvhtzxzqqjkmpb") == true)
    assert(solver.isReallyNice("xxyxx") == true)
    assert(solver.isReallyNice("uurcxstgmygtbstg") == false)
    assert(solver.isReallyNice("ieodomkazucvgmuy") == false)

    var result = 0
    val filename = "day5_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      if (solver.isReallyNice(line)) {
        result += 1 
      }
    }

    assert(result == 53)    
  }
}