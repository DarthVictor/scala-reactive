package simulations

import common._

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

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val a1Inv, a2Inv, a1InvAnda2Inv = new Wire
    inverter(a1, a1Inv)
    inverter(a2, a2Inv)
    andGate(a1Inv, a2Inv, a1InvAnda2Inv)
    inverter(a1InvAnda2Inv, output)
  }

 
    def demux(in: Wire, c: List[Wire], out: List[Wire]) {
      c match {
        case head :: Nil => {
            val headInv = new Wire
            inverter(head, headInv)
            andGate(head, in, out(0))
            andGate(headInv, in, out(1))
        }
        
        case head :: tail => {
            val headInv, headOut, headInvOut = new Wire
            inverter(head, headInv)
            andGate(head, in, headOut)
            andGate(headInv, in, headInvOut)
            val (outs, outsInv) = out.splitAt(out.length/2)
            demux(headOut, tail, outs)
            demux(headInvOut, tail, outsInv)
        }
        
        case Nil =>
        case _ =>
      }
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

  def orGateExample {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in2.setSignal(true)
    run
  }
  
  def orGate2Example {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

  }
  
  def demuxExample {
    val in, c0, out0, out1 = new Wire
    demux(in, List(c0), List(out1, out0))
    probe("in", in)
    probe("c0", c0)
    probe("out0", out0)
    probe("out1", out1)
    
    in.setSignal(true)
    run

    c0.setSignal(true)
    run

    in.setSignal(false)
    run
  }
  
  def demuxExample2 {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c1, c0), List(out3, out2, out1, out0))
    probe("in", in)
    probe("c0", c0)
    probe("c1", c1)
    probe("out0", out0)
    probe("out1", out1)
    probe("out2", out2)
    probe("out3", out3)
    
    in.setSignal(true)
    run

    c1.setSignal(true)
    run
    
    c0.setSignal(true)
    run

    c1.setSignal(false)
    run
    
    in.setSignal(false)
    run
  }
  
  def demuxExample3 {
    val in, c2, c1, c0, out7, out6, out5, out4, out3, out2, out1, out0 = new Wire
    demux(in, List(c2, c1, c0), List(out7, out6, out5, out4, out3, out2, out1, out0))
    probe("in", in)
    probe("c0", c0)
    probe("c1", c1)
    probe("c2", c2)
    probe("out0", out0)
    probe("out1", out1)
    probe("out2", out2)
    probe("out3", out3)
    probe("out4", out4)
    probe("out5", out5)
    probe("out6", out6)
    probe("out7", out7)
    
    in.setSignal(true)
    run

    c1.setSignal(true)
    run
    
    c0.setSignal(true)
    run

    c1.setSignal(false)
    run
    
    c2.setSignal(true)
    run
    
    c1.setSignal(true)
    run
    
    in.setSignal(false)
    run
  }
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  /* 
  Circuit.andGateExample
  Circuit.orGateExample
  Circuit.orGate2Example

  Circuit.demuxExample
  Circuit.demuxExample2
    */
  Circuit.demuxExample3
}
