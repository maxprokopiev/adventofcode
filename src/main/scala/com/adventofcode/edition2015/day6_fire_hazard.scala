package com.adventofcode.edition2015

class FireHazard {
  type OnOffGrid = Array[Array[Boolean]]
  type BrightnessGrid = Array[Array[Int]]

  type ChangeGridCallback[T] = (Int, Int, T) => Unit

  var onOffGrid = Array.fill(1000, 1000){false}
  var brightnessGrid = Array.fill(1000, 1000){0}

  def litLights: Int = {
    this.onOffGrid.foldLeft(0)((sum: Int, row: Array[Boolean]) => {
      sum + row.foldLeft(0)((rowSum: Int, elem: Boolean) => { if (elem) { rowSum + 1 } else rowSum })
    })
  }

  def totalBrightness: Int = {
    this.brightnessGrid.foldLeft(0)((sum: Int, row: Array[Int]) => {
      sum + row.foldLeft(0)((rowSum: Int, elem: Int) => { rowSum + elem })
    })
  }

  def changeGrid[T](grid: T, point1: Int, point2: Int, point3: Int, point4: Int, f: ChangeGridCallback[T]): T = {
    for (i <- point1 to point3) {
      for (j <- point2 to point4) {
        f(i, j, grid)
      }
    }
    grid
  }

  def applyInstruction(instruction: String): Unit = {
    val turnOn = """turn on (\d+),(\d+) through (\d+),(\d+)""".r
    val turnOff = """turn off (\d+),(\d+) through (\d+),(\d+)""".r
    val toggle = """toggle (\d+),(\d+) through (\d+),(\d+)""".r

    instruction match {
      case turnOn(point1, point2, point3, point4) => {
        changeGrid[OnOffGrid](this.onOffGrid, point1.toInt, point2.toInt, point3.toInt, point4.toInt, (i: Int, j: Int, grid: OnOffGrid) => {
          grid(i)(j) = true
        })

        changeGrid[BrightnessGrid](this.brightnessGrid, point1.toInt, point2.toInt, point3.toInt, point4.toInt, (i: Int, j: Int, grid: BrightnessGrid) => {
          grid(i)(j) += 1
        })
      }
      case turnOff(point1, point2, point3, point4) => {
        changeGrid[OnOffGrid](this.onOffGrid, point1.toInt, point2.toInt, point3.toInt, point4.toInt, (i: Int, j: Int, grid: OnOffGrid) => {
          grid(i)(j) = false
        })

        changeGrid[BrightnessGrid](this.brightnessGrid, point1.toInt, point2.toInt, point3.toInt, point4.toInt, (i: Int, j: Int, grid: BrightnessGrid) => {
          grid(i)(j) -= 1
          grid(i)(j) = if (grid(i)(j) < 0) 0 else grid(i)(j)
        })
      }
      case toggle(point1, point2, point3, point4) => {
        changeGrid[OnOffGrid](this.onOffGrid, point1.toInt, point2.toInt, point3.toInt, point4.toInt, (i: Int, j: Int, grid: OnOffGrid) => {
          grid(i)(j) = if (grid(i)(j)) false else true
        })

        changeGrid[BrightnessGrid](this.brightnessGrid, point1.toInt, point2.toInt, point3.toInt, point4.toInt, (i: Int, j: Int, grid: BrightnessGrid) => {
          grid(i)(j) += 2
        })
      }
    }
  }
}
