package com.adventofcode.edition2015

class JSAbacusFramework {
  def sum(document: String): Int = {
    val Number = """-?\d+""".r
    Number.findAllMatchIn(document).toList.map(_.toString.toInt).sum
  }
}
