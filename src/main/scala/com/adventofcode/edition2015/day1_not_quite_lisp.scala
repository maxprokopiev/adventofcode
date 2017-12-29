package com.adventofcode.edition2015

class NotQuiteLisp {
  def findFloor(path: String): Int = {
    path.foldLeft(0)( (acc: Int, c: Char) => {
      c match {
        case '(' => acc + 1
        case ')' => acc - 1
      }
    })
  }

  def firstTimeBasement(path: String): Int = {
    var acc = 0

    for ((char, index) <- path.toSeq.zipWithIndex) {
       char match {
        case '(' => acc += 1
        case ')' => acc -= 1
      }   
      
      if (acc == -1) {
        return index + 1
      }   
    }
    
    0
  }
}