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
  
   override def toString = if (sigVal) "1" else "0"
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
  
  /**
   * Helper method for relaying an input signal
   */
  def identity(in: Wire, out: Wire) {
    def identityAction() {
      val inSig = in.getSignal
      afterDelay(0) { out.setSignal(inSig) }
    }
    
    in addAction identityAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val notA1, notA2, notOut = new Wire
    inverter(a1, notA1)
    inverter(a2, notA2)
    andGate(notA1, notA2, notOut)
    inverter(notOut, output)
  }

  /**
   * Helper method for demux-ing a single control bit
   */
  def bitDemux(in: Wire, control: Wire, out1: Wire, out0: Wire) {
    
    def bitDemuxAction() {
      
      val inSig = in.getSignal
      val controlSig = control.getSignal
      
      afterDelay(0) {
        
        controlSig match {
          
          case false => {
            out0.setSignal(inSig)
            out1.setSignal(false)
          }
          case true  => {
            out0.setSignal(false)
            out1.setSignal(inSig)
          }
        }
      }
    }
    
    in addAction bitDemuxAction
    control addAction bitDemuxAction
  }

  /**
   * @param in - input wire
   * @param c - control wires (of length n)
   * @param out - output wires (of length 2^n -1)
   */
  def demux(in: Wire, cs: List[Wire], out: List[Wire]) {
    
    require(out.length == math.pow(2, cs.size), "Must have 2^n output wires")
    
    cs match {
      
      // no control wires
      case Nil => identity(in, out.head)
      
      // single control wire
      case c :: Nil => {
        
        val control = cs.head
        val out1 = out.head 
        val out0 = out.tail.head
        
        bitDemux(in, control, out1, out0)
      }
      
      // n control wires
      case c :: tail => {
        
        val out0 = new Wire
        val out1 = new Wire
        
        bitDemux(in, c, out1, out0)
        val split = out.splitAt(out.size/2)
        
        demux(out1, tail, split._1)
        demux(out0, tail, split._2)
        
      }  
    }
    
  }

}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  val IdentityGateDelay = 0
  val BitDemuxDelay = 0

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
