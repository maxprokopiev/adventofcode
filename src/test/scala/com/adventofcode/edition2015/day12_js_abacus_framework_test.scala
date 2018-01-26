package com.adventofcode.edition2015

import org.scalatest._
import scala.io.Source

class JSAbacusFrameworkTest extends FlatSpec {
  it should "calculate the sum of all numbers in a document" in {
    val solver = new JSAbacusFramework

    val filename = "day12_input.txt"
    assert(solver.sum(Source.fromResource(filename).getLines.toList(0)) == 111754)
  }
}
