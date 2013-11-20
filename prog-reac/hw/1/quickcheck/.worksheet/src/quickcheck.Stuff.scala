package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

object Stuff {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(334); 

  def genHeap: Gen[QuickCheckHeap.H] = for {
    v <- arbitrary[Int] // generate some arbitrary int 'v' to insert
    h <- oneOf(value(empty), genHeap) // choose an empty or generated heap
  } yield insert(v, h);System.out.println("""genHeap: => org.scalacheck.Gen[<error>]""")}
  
}
