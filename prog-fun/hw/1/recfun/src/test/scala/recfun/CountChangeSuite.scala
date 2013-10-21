package recfun

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CountChangeSuite extends FunSuite {
  import Main.countChange
  
  /*
   * Provided test cases
   */
  test("countChange: example given in instructions") {
    assert(countChange(4,List(1,2)) === 3)
  }

  test("countChange: sorted CHF") {
    assert(countChange(300,List(5,10,20,50,100,200,500)) === 1022)
  }

  test("countChange: no pennies") {
    assert(countChange(301,List(5,10,20,50,100,200,500)) === 0)
  }

  test("countChange: unsorted CHF") {
    assert(countChange(300,List(500,5,50,100,20,200,10)) === 1022)
  }
  
  /*
   * Added test cases
   */
  test("countChange: negative money amount") {
    assert(countChange(-1, List(1,2,3,4,5)) == 0)
  }
  
  test("countChange: negative money, no coins") {
    assert(countChange(-1, List()) == 0)
  }
  
  test("countChange: positive money, no valid coins to make it with") {
    assert(countChange(1, List(2)) == 0)
  }
  
  test("countChange: zero money, no coins") {
    assert(countChange(0, List()) == 1)
  }
  
  test("countChange: zero money, some coins") {
    assert(countChange(0, List(1, 2, 3)) == 1)
  }
  
  test("countChange: one way") {
    assert(countChange(1, List(1, 2)) == 1)
  }
  
  test("countChange: two ways") {
    assert(countChange(2, List(1, 2)) == 2)
  }
  
  test("countChange: two ways yet again") {
    assert(countChange(3, List(1, 2)) == 2)
  }
  
  test("countChange: three ways") {
    assert(countChange(4, List(1, 2)) == 3)
  }
  
  test("countChange: several ways") {
    assert(countChange(10, List(1, 2, 5, 10)) == 11)
  }
  
  test("countChange: several ways, on mo' gain with diff ordering") {
    assert(countChange(10, List(5, 10, 2, 1)) == 11)
  }
}
