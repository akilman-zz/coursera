package week01

object session {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(77); 

  def abs(x: Double) = if (x < 0) -x else x;System.out.println("""abs: (x: Double)Double""");$skip(313); 


  def sqrt(x: Double) = {
  
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  };System.out.println("""sqrt: (x: Double)Double""");$skip(11); val res$0 = 

  sqrt(2);System.out.println("""res0: Double = """ + $show(res$0));$skip(10); val res$1 = 
  sqrt(4);System.out.println("""res1: Double = """ + $show(res$1));$skip(13); val res$2 = 
  sqrt(1e-6);System.out.println("""res2: Double = """ + $show(res$2));$skip(13); val res$3 = 
  sqrt(1e60);System.out.println("""res3: Double = """ + $show(res$3));$skip(220); 
  
  // Assignment 1, exercise 1
  def pascal(c: Int, r: Int): Int =
  
  	if (c < 0 || r < 0)
  		0
  	else
  		if ((c == 1 && r == 1) || (c == 0 || r == 0))
  			1
  		else
  			pascal(c - 1, r - 1) + pascal(c, r - 1);System.out.println("""pascal: (c: Int, r: Int)Int""");$skip(16); val res$4 = 
  pascal(0, -1);System.out.println("""res4: Int = """ + $show(res$4));$skip(16); val res$5 = 
  pascal(-1, 0);System.out.println("""res5: Int = """ + $show(res$5));$skip(17); val res$6 = 
  pascal(-1, -1);System.out.println("""res6: Int = """ + $show(res$6));$skip(14); val res$7 = 
  pascal(0,0);System.out.println("""res7: Int = """ + $show(res$7));$skip(14); val res$8 = 
  pascal(0,1);System.out.println("""res8: Int = """ + $show(res$8));$skip(14); val res$9 = 
  pascal(0,2);System.out.println("""res9: Int = """ + $show(res$9));$skip(14); val res$10 = 
  pascal(0,3);System.out.println("""res10: Int = """ + $show(res$10));$skip(14); val res$11 = 
  pascal(1,2);System.out.println("""res11: Int = """ + $show(res$11));$skip(14); val res$12 = 
  pascal(1,3);System.out.println("""res12: Int = """ + $show(res$12));$skip(14); val res$13 = 
  pascal(0,2);System.out.println("""res13: Int = """ + $show(res$13));$skip(131); 
  
  // Assignment 1, exercise 2
  // WIP
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true else false
  };System.out.println("""balance: (chars: List[Char])Boolean""");$skip(299); 
    
 	// Lecture 2.2, Currying syntax
 	
 	/**
 	 * Applies function f from a..b. Tail recursive.
 	 */
 	def sumTailRecursive(f: Int => Int) (a: Int, b: Int): Int = {
 	
 	  def loop(a: Int, acc: Int): Int = {
 	    if (a > b) acc
 	    else loop(a + 1, f(a) + acc)
 	  }
 	  
 	  loop(a, 0)
 	 };System.out.println("""sumTailRecursive: (f: Int => Int)(a: Int, b: Int)Int""");$skip(170); 
 	
 	
 	
 	/**
   * Applies function f from a..b. Not tail recursive.
   */
 	def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b);System.out.println("""sum: (f: Int => Int)(a: Int, b: Int)Int""");$skip(30); val res$14 = 
 	
 	sum (x => x * x) (1, 10);System.out.println("""res14: Int = """ + $show(res$14))}
 	
}
