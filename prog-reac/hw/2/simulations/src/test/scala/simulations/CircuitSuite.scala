package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
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
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
  test("orGate2") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")
    
    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")
    
    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
  test("identityGate") {
    val in, out = new Wire
    identity(in, out)
    
    in.setSignal(true)
    run
    
    assert(out.getSignal === true, "true case")
    
    in.setSignal(false)
    run
    
    assert(out.getSignal === false, "false case")
  }
  
  test("bitDemux") {
    val in, c0, o0, o1 = new Wire
    bitDemux(in, c0, o1, o0)
    
    in.setSignal(true)
    run
    
    assert(o0.getSignal === true, "Control set to 0 - o0")
    assert(o1.getSignal === false, "Control set to 0 - o1")
    
    in.setSignal(true)
    c0.setSignal(true)
    run
    
    assert(o0.getSignal === false, "Control set to 1 - o0")
    assert(o1.getSignal === true, "Control set to 1 - o1")
  }
  
  test("demux single control entry case") {
    val in, c0, o0, o1 = new Wire
    val cs = c0 :: Nil
    val out = o1 :: o0 :: Nil
    
    demux(in, cs, out)
    
    in.setSignal(true)
    run
    
    assert(o0.getSignal === true, "Control set to 0 - o0")
    assert(o1.getSignal === false, "Control set to 0 - o1")
    
    in.setSignal(true)
    c0.setSignal(true)
    run
    
    assert(o0.getSignal === false, "Control set to 1 - o0")
    assert(o1.getSignal === true, "Control set to 1 - o1")
    
  }
  
  test("demux multiple output case") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val cs = c1 :: c0 :: Nil
    val out = o3 :: o2 :: o1 :: o0 :: Nil
    demux(in, cs, out)
    
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)
    
    in.setSignal(true)
    run
    
    assert(o0.getSignal === true)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)
    
    in.setSignal(true)
    c0.setSignal(true)
    run
    
    assert(o0.getSignal === false)
    assert(o1.getSignal === true)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)
    
    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === true)
    
    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === true)
    assert(o3.getSignal === false)
  }

}
