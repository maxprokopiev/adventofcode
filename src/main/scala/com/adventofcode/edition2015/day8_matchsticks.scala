package com.adventofcode.edition2015

class Matchsticks {
  def encodeString(s: String): String = {
    s.foldLeft("")( (result: String, c: Char) => {
      c match {
        case '"'  => { result + "\\\"" }
        case '\\' => { result + "\\\\" }
        case _    => { result + c }
      }
    })
  }

  class Pair(val count: Int, val context: Symbol)

  def inMemoryStringSizeFor(s: String): Int = {
    val hex = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

    s.foldLeft(new Pair(0, 'start))((pair: Pair, c: Char) => {
      c match {
        case elem if hex.contains(elem) => {
          pair.context match {
            case 'x => { new Pair(pair.count, 'hex) }
            case _  => { new Pair(pair.count + 1, 'done) }
          }
        }
        case '\\' => { 
          pair.context match {
            case 'esc => { new Pair(pair.count + 1, 'done) }
            case 'x   => { new Pair(pair.count + 1, 'esc) }
            case 'hex => { new Pair(pair.count + 3, 'esc) }
            case _    => { new Pair(pair.count, 'esc) }
          }
        }
        case 'x' => {
          pair.context match {
            case 'esc => { new Pair(pair.count, 'x) }
            case _    => { new Pair(pair.count + 1, 'done) }
          }
        }
        case _ => { new Pair(pair.count + 1, 'done) }
      }
    }).count
  }
}