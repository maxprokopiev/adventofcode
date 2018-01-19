package com.adventofcode.edition2015

import org.scalatest._
import scala.io.Source

class DeliveryTest extends FlatSpec {
  it should "calculate the distance of the shortest route" in {
    val solver = new Delivery

    solver.addDistance("London to Dublin = 464")
    solver.addDistance("London to Belfast = 518")
    solver.addDistance("Dublin to Belfast = 141")

    assert(solver.shortestRoute == 605)

  }

  it should "calculate the distance of the shortest route reading from a file" in {
    val solver = new Delivery

    val filename = "day9_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      solver.addDistance(line)
    }

    assert(solver.shortestRoute == 117)
  }

  it should "calculate the distance of the longest route reading from a file" in {
    val solver = new Delivery

    val filename = "day9_input.txt"
    for (line <- Source.fromResource(filename).getLines) {
      solver.addDistance(line)
    }

    assert(solver.longestRoute == 909)
  }
}
