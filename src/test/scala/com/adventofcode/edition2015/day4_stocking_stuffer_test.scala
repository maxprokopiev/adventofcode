package com.adventofcode.edition2015
import org.scalatest._

class StockingStufferTest extends FlatSpec {
  it should "properly mine AdventCoins" in {
    val solver = new StockingStuffer

    assert(solver.findNumberFor("abcdef", "00000") == 609043)
    assert(solver.findNumberFor("pqrstuv", "00000") == 1048970)
    assert(solver.findNumberFor("yzbqklnj", "00000") == 282749)
    assert(solver.findNumberFor("yzbqklnj", "000000") == 9962624)
  }
}