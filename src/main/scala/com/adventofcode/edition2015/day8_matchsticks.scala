package com.adventofcode.edition2015

class Matchsticks {
  def inMemoryStringSizeFor(s: String): Int = {
    val hex = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')
    var context = 'start
    var count = 0

    for (c <- s)
      c match {
        case elem if hex.contains(elem) => {
          context match {
            case 'x' => { context = 'hex1 }
            case 'hex1 => {
              context = 'done
              count += 1
            }
            case _ => { count += 1 }
          }
        }
        case '\\' => { 
          context = 'escape
        }
        case '"' => {
          context match {
            case 'escape => {
              context = 'done
              count += 1
            }
            case _ => { count += 1 }
          }
        }
        case 'x' => {
          context = 'hex
        }
        case _ => {
          count += 1
        }
      }

    count
  }
}