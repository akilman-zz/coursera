package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalanceSuite extends FunSuite {
  import Main.balance

  /*
   * Provided test cases
   */
  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }
  
  test("balance: negative, balanced, but reversed") {
    assert(!balance(")(".toList))
  }
  
  /*
   * Added test cases
   */
  test("balance: negative, larger, mismatched") {
    assert(!balance("((()()".toList))
  }
  
  test("balance: positive, empty") {
    assert(balance("".toList))
  }
  
  test("balance: positive, but a larger entity") {
    assert(balance("((((((((()(())))()()()))))))".toList))
  }
  
  test("balance: negative, but larger quantity") {
    assert(!balance("((((((((((((((((((((".toList))
  }
  
}
