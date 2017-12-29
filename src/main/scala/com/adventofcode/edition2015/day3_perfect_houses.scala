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

    def moveTo(direction: Char): Point = {
      direction match {
        case '>' => this.toEast
        case '^' => this.toNorth
        case '<' => this.toWest
        case 'v' => this.toSouth
      }    
    }

    override def toString: String = {
      s"($x,$y)"
    }
  }

  private def visitHouse(houses: Map[String, Int], position: Point): Map[String, Int] = {
    houses + (position.toString -> (houses.getOrElse(position.toString, 0) + 1))
  }  

  def calculateNumberOfHouses(directions: String): Int = {
    val position = new Point(0, 0)

    directions.foldLeft((Map(position.toString -> 1), position))( (houses: (Map[String, Int], Point), direction: Char) => {
      val newPosition = houses._2.moveTo(direction)
      (visitHouse(houses._1, newPosition), newPosition)
    })._1.keySet.size
  }

  def calculateNumberOfHousesWithRoboSanta(directions: String): Int = {
    val position = new Point(0, 0)

    directions.grouped(2).foldLeft((Map(position.toString -> 1), position, position))( (houses: (Map[String, Int], Point, Point), directions: String) => {
      val newSantaPosition = houses._2.moveTo(directions(0))
      val newRoboSantaPosition = houses._3.moveTo(directions(1))

      (
       visitHouse(visitHouse(houses._1, newSantaPosition), newRoboSantaPosition),
       newSantaPosition,
       newRoboSantaPosition
      )
    })._1.keySet.size  
  }
}