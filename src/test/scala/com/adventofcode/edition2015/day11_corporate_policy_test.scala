package com.adventofcode.edition2015

import org.scalatest._

class CorporatePolicyTest extends FlatSpec {
  it should "provide the correct next password" in {
    val solver = new CorporatePolicy

    assert(solver.nextPassword("vzbxkghb") == "vzbxxyzz")
    assert(solver.nextPassword("vzbxxyzz") == "vzcaabcc")
  }
}
