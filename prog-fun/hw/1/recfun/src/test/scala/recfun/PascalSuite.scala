package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PascalSuite extends FunSuite {
  import Main.pascal
  
  /*
   * Provided test cases
   */
  test("pascal: col=0,row=2") {
    assert(pascal(0,2) === 1)
  }

  test("pascal: col=1,row=2") {
    assert(pascal(1,2) === 2)
  }

  test("pascal: col=1,row=3") {
    assert(pascal(1,3) === 3)
  }

  /*
   * Added test cases
   */
  test("pascal: col=-1,row=-1") {
    assert(pascal(-1, -1) === 0)
  }

  test("pascal: col=-1,row=0") {
    assert(pascal(-1, 0) === 0)
  }

  test("pascal: col=0,row=-1") {
    assert(pascal(0, -1) === 0)
  }

  test("pascal: col=0,row=0") {
    assert(pascal(0, 0) === 1)
  }

  test("pascal: col=0,row=1") {
    assert(pascal(0, 1) === 1)
  }

  test("pascal: col=0,row=3") {
    assert(pascal(0, 3) === 1)
  }

  // TODO - Ask in group about these
  test("pascal: col=1,row=0") {
    assert(pascal(1, 0) === 0)
  }

  test("pascal: col=2,row=0") {
    assert(pascal(2, 0) === 0)
  }

  test("pascal: col=2,row=4") {
    assert(pascal(2, 4) === 6)
  }
}