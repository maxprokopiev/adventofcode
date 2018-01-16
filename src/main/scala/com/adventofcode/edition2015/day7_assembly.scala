package com.adventofcode.edition2015

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class Assembly {
  class Gate {
    var name: String = ""
    var signal: Int = -1
    var operation: String = ""
    var inputs = ArrayBuffer[Gate]()
  }

  def newGate(name: String): Gate = {
    var gate = new Gate
    gate.name = name  

    val intRegex = """(\d+)""".r
    name match {
      case intRegex(signal) => { gate.signal = signal.toInt }
      case _ => {}
    }

    gates(name) = gate
    gate
  }

  var gates = Map[String, Gate]().withDefault(newGate)

  def applyInstruction(s: String): Unit = {
    val Signal = """(\w+) -> (\w+)""".r
    val UnaryOp = """NOT (\w+) -> (\w+)""".r
    val BinaryOp = """(\w+) (OR|AND|RSHIFT|LSHIFT) (\w+) -> (\w+)""".r

    s match {
      case Signal(arg1, receiver) => {
        println("signal")

        gates(receiver).operation = "SIGNAL"     
        gates(receiver).inputs += gates(arg1)          
      }

      case UnaryOp(arg1, receiver) => {
        println("unary op")

        gates(receiver).operation = "NOT"
        gates(receiver).inputs += gates(arg1)        
      }

      case BinaryOp(arg1, operation, arg2, receiver) => {
        println(s"binary op $operation")

        gates(receiver).operation = operation
        gates(receiver).inputs += gates(arg1)
        gates(receiver).inputs += gates(arg2)      
      }
    }
  }

  def signalOn(s: String): Int = {
    print(s"calculating signal on $s... ")
    
    if (gates(s).signal == -1) {
      println("")

      gates(s).operation match {
        case "SIGNAL" => { gates(s).signal = signalOn(gates(s).inputs(0).name) }
        case "NOT"    => { gates(s).signal = (65535 + ~signalOn(gates(s).inputs(0).name) + 1) % 65535 }
        case "OR"     => { gates(s).signal = signalOn(gates(s).inputs(0).name) | signalOn(gates(s).inputs(1).name) }
        case "AND"    => { gates(s).signal = signalOn(gates(s).inputs(0).name) & signalOn(gates(s).inputs(1).name) }
        case "LSHIFT" => { gates(s).signal = signalOn(gates(s).inputs(0).name) << signalOn(gates(s).inputs(1).name) }
        case "RSHIFT" => { gates(s).signal = signalOn(gates(s).inputs(0).name) >> signalOn(gates(s).inputs(1).name) }      
        case _        => { gates(s).signal }
      }
    } else {
      println("signal exists")
    }

    gates(s).signal
  }
}