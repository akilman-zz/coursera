package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    final val s1 = singletonSet(1)
    final val s2 = singletonSet(2)
    final val s3 = singletonSet(3) 
    final val emptySet = intersect(s1, s2)
    final val positiveIntegers = (x: Int) => x > 0
    final val negativeIntegers = (x: Int) => x < 0
  }

  test("singletonSet") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 2), "Singleton, other values not in set")
      assert(!contains(s1, 0), "Singleton, other values not in set")
    }
  }

  test("union") {
    new TestSets {
      
      // provided
      val s = union(s1, s2)
      assert(contains(s, 1))
      assert(contains(s, 2))
      assert(!contains(s, 3))

      // merging a range
      val a = range(1,5)
      val b = range(6, 10)
      val ab = union(a, b)
      assert(!contains(ab, 0))
      for (i <- 1 to 10) assert(contains(ab, i))
      assert(!contains(ab, 11))
    }
  }
  
  test("intersect") {
    new TestSets {
      // provided
      final val oneAndTwo = union(s1, s2)
      assert(contains(intersect(s1, oneAndTwo), 1), "Insersect (1), (1,2)")
      
      // disjoint singletons for a mock empty set
      assert(!contains(emptySet, 1))
      assert(!contains(emptySet, 2))
      
      // disjoint ranges for a different empty set
      val s = intersect(range(1,5), range(6,10))
      for (i <- 1 to 10) assert(!contains(s, i))
    }
  }
  
  test("filter") {
    new TestSets {
	    final val anotherEmptySet = filter(s1, (x: Int) => (x != 1))
	    assert(!contains(anotherEmptySet, 0))
	    assert(!contains(anotherEmptySet, 1))
	    assert(!contains(anotherEmptySet, 2))
	    
	    final val notTwo = (x: Int) => x != 2
	    final val allPositiveIntegersButTwo = filter(positiveIntegers, notTwo)
	    assert(contains(allPositiveIntegersButTwo, 1))
	    assert(!contains(allPositiveIntegersButTwo, 2))
	    assert(contains(allPositiveIntegersButTwo, 3))
	    assert(contains(allPositiveIntegersButTwo, 4))
	    
    }
  }
  
  test("forall") {
    new TestSets {
      
      // set contained within the predicate
      final val greaterThanTwo = (x: Int) => x > 2
      assert(forall(greaterThanTwo, positiveIntegers))
      
      // set not contained within the predicate
      assert(!forall(negativeIntegers, positiveIntegers))
      
      // set partially contained within the predicate
      assert(!forall(range(10, 20), range(15, 25)))
    }
  }

  test("exists") {
    new TestSets{
      // right-hand overlap
      assert(exists(range(10, 20), range(15, 25)))

      // right-hand disjoint
      assert(!exists(range(1,5), range(6,10)))
      
      // left-hand overlap
      assert(exists(range(15, 25), range(10, 20)))
      
      // left-hand disjoint
      assert(!exists(range(6,10), range(1,5)))
    }
  }
  
  test("map") {
    new TestSets {
      
      // s = x > 0, f = x^2
      // 1, 2, 3, 4, 5...
      // 1, 4, 9, 16, 25...
      final val perfectSquaresSet = map((x: Int) => x > 0, (x: Int) => x * x)
      assert(contains(perfectSquaresSet, 1))
      assert(!contains(perfectSquaresSet, 2))
      assert(contains(perfectSquaresSet, 4))
      assert(contains(perfectSquaresSet, 9))
      
      // s = x % 2, f = x^2
      // 0, 2, 4, 6, 8, ...
      // 0, 4, 16, 36, 64, ...
      final val evenNumbers = 
        map((x: Int) => x % 2 == 0 && x >= 0, (x: Int) => x * x)
      assert(contains(evenNumbers, 0))
      assert(!contains(evenNumbers, 1))
      assert(!contains(evenNumbers, 2))
      assert(contains(evenNumbers, 4))
      assert(contains(evenNumbers, 16))
      
      // s = x > 0, f = 0
      final val effectiveEmptySet = map((x: Int) => x > 0, (x: Int) => 0)
      assert(!contains(effectiveEmptySet, -1))
      assert(contains(effectiveEmptySet, 0))
      assert(!contains(effectiveEmptySet, 1))
    }
  }
  
}
