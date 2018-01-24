package com.adventofcode.edition2015

import org.scalatest._

class LookAndSayTest extends FlatSpec {
  it should "build a 'look-and-say' sequence from a string" in {
    val solver = new LookAndSay

    // assert(solver.buildWithRe("1") == "11")
    // assert(solver.buildWithRe("11") == "21")
    // assert(solver.buildWithRe("21") == "1211")
    // assert(solver.buildWithRe("1211") == "111221")
    // assert(solver.buildWithRe("111221") == "312211")

    val r = (1 to 40).foldLeft("1321131112")((s, index) => {
      println(index)
      solver.build(s)
    })

    assert(r.size == 1)
  }
}
