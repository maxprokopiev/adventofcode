package com.adventofcode.edition2015

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class Delivery {

  def newValue(name: String): ArrayBuffer[Pair] = {
    var emptyArray = ArrayBuffer[Pair]()
    cities(name) = emptyArray

    emptyArray
  }

  type Pair = (String, Int)
  var cities = Map[String, ArrayBuffer[Pair]]().withDefault(newValue)

  def addDistance(s: String): Unit = {
    val Distance = """(\w+) to (\w+) = (\d+)""".r

    s match {
      case Distance(city1, city2, distance) => {
        cities(city1) += Pair(city2, distance.toInt)
        cities(city2) += Pair(city1, distance.toInt)
      }
      case _ => {}
    }
  }

  def longestRoute: Int = {
    var distances = ArrayBuffer[Int]()

    for (route <- permutations(cities.keys.toList)) {
      val d = route.sliding(2).foldLeft(0)((totalDistance, pair) => {
        totalDistance + findDistance(pair)
      })
      distances += d
    }
    distances.max
  }

  def shortestRoute: Int = {
    var distances = ArrayBuffer[Int]()

    for (route <- permutations(cities.keys.toList)) {
      val d = route.sliding(2).foldLeft(0)((totalDistance, pair) => {
        totalDistance + findDistance(pair)
      })
      distances += d
    }
    distances.min
  }

  private def findDistance(pair: List[String]): Int = {
    val distance = cities(pair(0)).find((e) => {
      e._1 == pair(1)
    })

    distance match {
      case Some(value) => { value._2 }
      case None => { -1 }

    }
  }

  private def permutations(list: List[String]): List[List[String]] = list match {
    case Nil => List(Nil)
    case List(a) => List(list)
    case _ =>
      (for(i <- list.indices.toList) yield {
        val (before, rest) = list.splitAt(i)
        val elem = rest.head
        val subpermutes = permutations(before ++ rest.tail)
        subpermutes.map(elem::_)
      }).flatten
  }
}
