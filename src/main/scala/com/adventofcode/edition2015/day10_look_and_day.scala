package com.adventofcode.edition2015

class LookAndSay {
  def build(s: String): String = {
    iterate(1, s.head, s.tail, "")
  }

  private def iterate(count: Int, head: Char, tail: String, result: String): String = {
    if (tail.size == 0) {
      result ++ count.toString ++ head.toString
    } else {
      if (head == tail.head) {
        iterate(count + 1, tail.head, tail.tail, result)
      } else {
        iterate(1, tail.head, tail.tail, result ++ count.toString ++ head.toString)
      }
    }
  }
}
