package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import scala.annotation.tailrec

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  val bools = List(false, true)

  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")
  }

  //
  // to complete with tests for orGate, demux, ...
  //

  test("orGate") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)

    for {
      from1 <- bools
      from2 <- bools
      to1 <- bools
      to2 <- bools
      if from1 ^ to1 || from2 ^ to2
      exFrom = from1 || from2
      exTo = to1 || to2
    } {

      in1.setSignal(from1)
      in2.setSignal(from2)
      run

      assert(out.getSignal === exFrom, s"(${from1}, ${from2}) -> (${to1}, ${to2}) : init")

      val startTime = currentTime

      in1.setSignal(to1)
      in2.setSignal(to2)
      run

      assert(out.getSignal === exTo, s"(${from1}, ${from2}) -> (${to1}, ${to2}) : value")
      assert(currentTime - startTime === OrGateDelay, s"(${from1}, ${from2}) -> (${to1}, ${to2}) : delay")
    }
  }

  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)

    for {
      from1 <- bools
      from2 <- bools
      to1 <- bools
      to2 <- bools
      if from1 ^ to1 || from2 ^ to2
      exFrom = from1 || from2
      exMid = to1 || from2
      exTo = to1 || to2
      exDelay = InverterDelay + AndGateDelay + { if (exFrom ^ exMid || exMid ^ exTo) InverterDelay else 0 }
    } {
      in1.setSignal(from1)
      in2.setSignal(from2)
      run

      assert(out.getSignal === exFrom, s"(${from1}, ${from2}) -> (${to1}, ${to2}) : init")

      val startTime = currentTime

      in1.setSignal(to1)
      in2.setSignal(to2)
      run

      assert(out.getSignal === exTo, s"(${from1}, ${from2}) -> (${to1}, ${to2}) : value")
      assert(currentTime - startTime === exDelay, s"(${from1}, ${from2}) -> (${to1}, ${to2}) : delay")
    }
  }

  test("demux") {
    @tailrec
    def addWires(count: Int, acc: List[Wire] = Nil): List[Wire] =
      if (count == 0) acc else addWires(count - 1, new Wire :: acc)

    @tailrec
    def determineOutputCount(bits: Int, acc: Int = 1): Int =
      if (bits == 0) acc else determineOutputCount(bits - 1, 2 * acc)

    val input = new Wire

    for {
      bits <- 0 to 1

      outputCount = determineOutputCount(bits)
      maxDelay = if (bits == 0) 0 else InverterDelay + bits * AndGateDelay

      controls = addWires(bits)
      outputs = addWires(outputCount)

      controlsWithBit = controls zip ((bits - 1) to 0 by -1) map { case (c, i) => (c, 1 << i)}
      outputsWithIndex = outputs zip ((outputCount - 1) to 0 by -1)
    } {
      demux(input, controls, outputs)

      for {
        signal <- bools
        selected <- 0 until outputCount
      } {
        val startTime = currentTime

        input.setSignal(signal)
        controlsWithBit foreach { case (c, b) => c.setSignal((selected & b) == b)}
        run

        val delay = currentTime - startTime
        assert(delay <= maxDelay, s"${selected} ${signal} : delay ${delay} > ${maxDelay}")

        outputsWithIndex foreach { case (o, i) =>
          assert(o.getSignal === (i == selected && signal), s"${selected} ${i} ${signal}")
        }
      }
    }
  }
}
