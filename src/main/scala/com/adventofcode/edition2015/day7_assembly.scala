package com.adventofcode.edition2015

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

class Assembly {
  class Gate {
    var name: String = ""
    var signal: Int = -1
    var operation: String = "SIGNALGATE"
    var inputs = ArrayBuffer[Gate]()
  }

  var gates = Map[String, Gate]()

  def applyInstruction(s: String): Unit = {
    val Signal = """(\w+) -> (\w+)""".r
    val UnaryOp = """NOT (\w+) -> (\w+)""".r
    val BinaryOp = """(\w+) (OR|AND|RSHIFT|LSHIFT) (\w+) -> (\w+)""".r

    s match {
      case Signal(arg1, receiver) => {
        println("signal")

        if (!gates.contains(receiver)) {
          gates(receiver) = new Gate
          gates(receiver).name = receiver
        }   
        gates(receiver).operation = "SIGNAL"     

        val intRegex = """(\d+)""".r
        arg1 match {
          case intRegex(signal) => {
            var signalGate = new Gate
            signalGate.signal = signal.toInt
            signalGate.name = signal
            gates(signal) = signalGate
            gates(receiver).inputs += signalGate          
          }
          case _ => {
            if (!gates.contains(arg1)) {
              gates(arg1) = new Gate
              gates(arg1).name = arg1
            }

            gates(receiver).inputs += gates(arg1)          
          }
        }
      }

      case UnaryOp(arg1, receiver) => {
        println("unary op")

        if (!gates.contains(receiver)) {
          gates(receiver) = new Gate
          gates(receiver).name = receiver
        }

        gates(receiver).operation = "NOT"
        
        if (!gates.contains(arg1)) {
          gates(arg1) = new Gate
          gates(arg1).name = arg1
        }

        gates(receiver).inputs += gates(arg1)        
      }

      case BinaryOp(arg1, operation, arg2, receiver) => {
        println(s"binary op $operation")

        if (!gates.contains(receiver)) {
          gates(receiver) = new Gate
          gates(receiver).name = receiver
        }
        gates(receiver).operation = operation

        val intRegex = """(\d+)""".r
        arg1 match {
          case intRegex(signal) => {
            var signalGate = new Gate
            signalGate.signal = signal.toInt
            signalGate.name = signal
            gates(signal) = signalGate
            gates(receiver).inputs += signalGate
          }
          case _ => {
            if (!gates.contains(arg1)) {
              gates(arg1) = new Gate
              gates(arg1).name = arg1
            }

            gates(receiver).inputs += gates(arg1)
          }
        }

        arg2 match {
          case intRegex(signal) => {
            var signalGate = new Gate
            signalGate.signal = signal.toInt
            signalGate.name = signal
            gates(signal) = signalGate            
            gates(receiver).inputs += signalGate
          }
          case _ => {
            if (!gates.contains(arg2)) {
              gates(arg2) = new Gate
              gates(arg2).name = arg2
            }          
            gates(receiver).inputs += gates(arg2)
          }
        }        
      }
    }
  }

  def signalOn(s: String): Int = {
    print(s"calculating signal on $s...")
    
    if (gates(s).signal == -1) {
      println("")

      gates(s).operation match {
        case "SIGNAL" => { gates(s).signal = signalOn(gates(s).inputs(0).name) }
        case "NOT"    => { gates(s).signal = (65535 + ~signalOn(gates(s).inputs(0).name) + 1) % 65535 }
        case "OR"     => { gates(s).signal = signalOn(gates(s).inputs(0).name) | signalOn(gates(s).inputs(1).name) }
        case "AND"    => { gates(s).signal = signalOn(gates(s).inputs(0).name) & signalOn(gates(s).inputs(1).name)  }
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