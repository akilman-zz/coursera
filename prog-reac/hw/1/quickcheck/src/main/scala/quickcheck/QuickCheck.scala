package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
 
  /**
   * 'findMin' inspired properties
   */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("new entry, new potential minimum") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    if (a < b) findMin(h) == a else findMin(h) == b
  }
  
  /**
   * 'deleteMin' inspired properties
   */
  property("removal of a single element heap must result in an empty heap") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }
  
  property("removal of a minimum in a heap really removes the minimum") = forAll { (a: Int, b: Int) =>    
    val h1 = insert(b, insert(a, empty))
    val h2 = deleteMin(h1)
    
    (a,b) match {
      case (a,b) if (a < b) => findMin(h2) == b                // a would have been removed, new min must be b
      case (a,b) if (a == b) => true                	       // could be either, return true by default
      case (a,b) if (b < a) => findMin(h2) == a                // b would have been removed, new min must be a
    }
  }
  
  /**
   * 'meld' inspired properties
   */
  property("meld two non-empty heaps, minimum of the two should win") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    
    (h1, h2) match {
      case (h1, h2) if (isEmpty(h1) && isEmpty(h2)) => isEmpty(h)
      case (h1, h2) if (isEmpty(h1)) => findMin(h2) == findMin(h)
      case (h1, h2) if (isEmpty(h2)) => findMin(h1) == findMin(h)
      case (h1, h2) => math.min(findMin(h1), findMin(h2)) == findMin(h)
    }
  }
  
  property("meld two heaps, where min from h1 is pushed to h2") = forAll { (h1: H, h2: H) =>
    
    /**
     * Helper method to determine if two heaps are equal to e/o
     */
    def equals(x: H, y: H):Boolean = {
      (x, y) match {
        case (x,y) if (isEmpty(x) && isEmpty(y)) => true
        case (x,y) if (isEmpty(x) || isEmpty(y)) => false
        case (x,y) => {
          findMin(x) == findMin(y) && equals(deleteMin(x), deleteMin(y))
        }
      }
    }
    
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val oldH = meld(h1, h2)							// meld h1 + h2
      
      val minH1 = findMin(h1)                           // determine h1 min
      val newH1 = deleteMin(h1)							// remove said min to create new h1
      val newH2 = insert(minH1, h2)                     // push new min to create new h2
      
      val newH = meld(newH1, newH2)                     // meld new h1 and new h2 to create new H
      equals(oldH, newH)                                // which of course must be equal to the original
    }
  }
  
  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[Int]                // generate some arbitrary int 'v' to insert
    h <- oneOf(value(empty), genHeap)  // choose an empty or generated heap
  } yield insert(v, h)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(value(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
