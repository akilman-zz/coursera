package week01

object session {

  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double


  def sqrt(x: Double) = {
  
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  sqrt(2)                                         //> res0: Double = 1.4142156862745097
  sqrt(4)                                         //> res1: Double = 2.000609756097561
  sqrt(1e-6)                                      //> res2: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res3: Double = 1.0000788456669446E30
  
  // Assignment 1, exercise 1
  def pascal(c: Int, r: Int): Int =
  
  	if (c < 0 || r < 0)
  		0
  	else
  		if ((c == 1 && r == 1) || (c == 0 || r == 0))
  			1
  		else
  			pascal(c - 1, r - 1) + pascal(c, r - 1)
                                                  //> pascal: (c: Int, r: Int)Int
  pascal(0, -1)                                   //> res4: Int = 0
  pascal(-1, 0)                                   //> res5: Int = 0
  pascal(-1, -1)                                  //> res6: Int = 0
  pascal(0,0)                                     //> res7: Int = 1
  pascal(0,1)                                     //> res8: Int = 1
  pascal(0,2)                                     //> res9: Int = 1
  pascal(0,3)                                     //> res10: Int = 1
  pascal(1,2)                                     //> res11: Int = 2
  pascal(1,3)                                     //> res12: Int = 3
  pascal(0,2)                                     //> res13: Int = 1
  
  // Assignment 1, exercise 2
  // WIP
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true else false
  }                                               //> balance: (chars: List[Char])Boolean
    
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
 	 }                                        //> sumTailRecursive: (f: Int => Int)(a: Int, b: Int)Int
 	
 	
 	
 	/**
   * Applies function f from a..b. Not tail recursive.
   */
 	def sum(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum(f)(a + 1, b)     //> sum: (f: Int => Int)(a: Int, b: Int)Int
 	
 	sum (x => x * x) (1, 10)                  //> res14: Int = 385
 	
}