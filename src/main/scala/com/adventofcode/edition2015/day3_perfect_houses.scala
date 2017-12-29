package com.adventofcode.edition2015

class PerfectHouses {
  class Point(val x: Int, val y: Int) {
    def toNorth: Point = {
      new Point(this.x, this.y + 1)
    }

    def toEast: Point = {
      new Point(this.x + 1, this.y)
    }

    def toSouth: Point = {
      new Point(this.x, this.y - 1)
    }

    def toWest: Point = {
      new Point(this.x - 1, this.y)
    }

    override def toString: String = {
      s"($x,$y)"
    }
  }

  def calculateNumberOfHouses(directions: String): Int = {
    val position = new Point(0, 0)

    directions.foldLeft((Map(position.toString -> 1), position))( (houses: (Map[String, Int], Point), direction: Char) => {
      val newPosition = direction match {
        case '>' => houses._2.toEast
        case '^' => houses._2.toNorth
        case '<' => houses._2.toWest
        case 'v' => houses._2.toSouth
      }
      (houses._1 + (newPosition.toString -> (houses._1.getOrElse(newPosition.toString, 0) + 1)), newPosition)
    })._1.keySet.size
  }
}