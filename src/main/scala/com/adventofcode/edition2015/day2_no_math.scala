package com.adventofcode.edition2015

class NoMath {
  def totalSquare(dimensions: String): Int = {
    val Array(l, w, h) = dimensions.split("x").map(_.toInt)

    boxSurfaceArea(l, w, h) + extra(l, w, h)
  }

  def totalFeetOfRibbon(dimensions: String): Int = {
    val Array(l, w, h) = dimensions.split("x").map(_.toInt)

    forWrapping(l, w, h) + forBow(l, w, h)  
  }

  private def boxSurfaceArea(l: Int, w: Int, h: Int): Int = {
    2*l*w + 2*w*h + 2*h*l
  }
  
  private def extra(l: Int, w: Int, h: Int): Int = {
    Array(l*w, l*h, w*h).min
  }

  private def forWrapping(l: Int, w: Int, h: Int): Int = {
    l * w * h
  }

  private def forBow(l: Int, w: Int, h: Int): Int = {
    Array(2*l + 2*w, 2*l + 2*h, 2*w + 2*h).min
  }
}