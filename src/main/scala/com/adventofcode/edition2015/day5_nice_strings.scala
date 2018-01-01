package com.adventofcode.edition2015

class NiceStrings {
  def isNice(s: String): Boolean = {
    var lastChar: Char = ' '
    var numberOfVowels = 0
    var hasDoubles = false

    val restrictedPairs = Array("ab", "cd", "pq", "xy")
    val vowels = Array('a', 'e', 'i', 'o', 'u')

    s.foreach((c: Char) => {
      if (vowels.indexOf(c) != -1) { numberOfVowels += 1 }
      if (restrictedPairs.indexOf(s"$lastChar$c") != -1) { return false }
      if (lastChar == c) { hasDoubles = true }

      lastChar = c
    })

    hasDoubles && (numberOfVowels > 2)
  }

  private def hasPairs(s: String): Boolean = {
    val pairs = s.sliding(2, 1).toList

    pairs.zipWithIndex.exists((e: (String, Int)) => {
      pairs.drop(e._2 + 2).indexOf(e._1) != -1
    })  
  }

  private def hasRepeatingChar(s: String): Boolean = {
    s.sliding(3, 1).exists((substr: String) => (substr.head == substr.last))
  }

  def isReallyNice(s: String): Boolean = {
    hasPairs(s) && hasRepeatingChar(s)
  }
}