package com.adventofcode.edition2015

class LookAndSay {
  def buildWithRe(s: String): String = {
    val re = """((\d+)\2+)|(\d)""".r
    var result = ""
    for (ss <- re.findAllMatchIn(s)) {
      val str = ss.toString
      if (str.size > 1) {
        result = result ++ str.size.toString ++ str(0).toString
      } else {
        result = result ++ "1" ++ str(0).toString
      }
    }

    result
  }

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

// 1
// 11
// 21
// 1211
// 111221
// 312211
