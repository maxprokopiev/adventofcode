package com.adventofcode.edition2015

class CorporatePolicy {
  def nextPassword(s: String): String = {
    var next = s
    do {
      next = iterate(next, next.size - 1)
    } while (!(pairs(next) && !restrictedLetters(next) && straightOfThree(next)))

    next
  }

  private def pairs(s: String): Boolean = {
    s.sliding(2).filter(pair => {
      pair(0) == pair(1)
    }).toList.distinct.size > 1
  }

  private def restrictedLetters(s: String): Boolean = {
    val re = """[iol]""".r
    re.findFirstMatchIn(s) match {
      case Some(_) => true
      case None => false
    }
  }

  // TODO: use exists instead
  private def straightOfThree(s: String): Boolean = {
    s.sliding(3).find(group => {
      ((group(0) + 1) == group(1)) && (group(1) + 1 == group(2))
    }) match {
      case Some(_) => true
      case None => false
    }
  }

  private def iterate(s: String, index: Int): String = {
    val next = (s(index) + 1) % 123

    if (next != 0) {
      s.substring(0, index) + next.toChar + s.substring(index + 1, s.size)
    } else {
      iterate(s.substring(0, index) + 'a' + s.substring(index + 1, s.size), index - 1)
    }
  }
}
