package simulations

import common._
import scala.annotation.tailrec

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  def andGateNewOutput(in1: Wire, in2: Wire) = {
    val out = new Wire
    andGate(in1, in2, out)
    out
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(in1: Wire, in2: Wire, output: Wire) {
    def orAction() {
      val outSig = in1.getSignal | in2.getSignal
      afterDelay(OrGateDelay)(output.setSignal(outSig))
    }
    in1 addAction orAction
    in2 addAction orAction
  }

  def orGate2(in1: Wire, in2: Wire, output: Wire) {
    val inv1, inv2, and = new Wire
    inverter(in1, inv1)
    inverter(in2, inv2)
    andGate(inv1, inv2, and)
    inverter(and, output)
  }

  def splice(in: Wire, out: Wire) {
    in addAction { () => out.setSignal(in.getSignal) }
  }

  def delay(delay: Int)(in: Wire, out: Wire) {
    in addAction { () =>
      val sig = in.getSignal
      afterDelay(delay)(out.setSignal(sig))
    }
  }

  def demux(in: Wire, controls: List[Wire], outputs: List[Wire]) {
    def addBit(inputs: List[Wire], c: Wire): List[Wire] = {
      val on, off = new Wire
      delay(InverterDelay)(c, on)
      inverter(c, off)
      inputs.flatMap { in => andGateNewOutput(in, on) :: andGateNewOutput(in, off) :: Nil }
    }

    controls.foldLeft (List(in)) (addBit) .
      view .
      zip(outputs) .
      foreach { case (a, b) => splice(a, b) }
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
