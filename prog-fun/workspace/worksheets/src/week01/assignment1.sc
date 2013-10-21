package week01

object assignment1 {

  /**
   * Exercise 1 - Pascal's triangle
   */
  def pascal(c: Int, r: Int): Int = {
  
    if (c < 0 || r < 0)
      0
    else
      if (c == 0 || r == c)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
  }                                               //> pascal: (c: Int, r: Int)Int
  
  // negative cases
  pascal(-1, -1)                                  //> res0: Int = 0
  pascal(-1, 0)                                   //> res1: Int = 0
  pascal(0, -1)                                   //> res2: Int = 0
  
  // positive cases
  pascal(0, 0)                                    //> res3: Int = 1
  pascal(0, 1)                                    //> res4: Int = 1
  pascal(0, 2)                                    //> res5: Int = 1
  pascal(0, 3)                                    //> res6: Int = 1
  pascal(1, 0)                                    //> res7: Int = 0
  pascal(2, 0)                                    //> res8: Int = 0
  pascal(3, 0)                                    //> res9: Int = 0
  
  // cited in exercise description
  pascal(0, 2)                                    //> res10: Int = 1
  pascal(1, 2)                                    //> res11: Int = 2
  pascal(1, 3)                                    //> res12: Int = 3
  pascal(2, 4)                                    //> res13: Int = 6
  
  /**
   * Exercise 2 - Parantheses balancing
   *
   * Recursively iterates through the list of characters, counting left
   * parens as +1 and right parens as -1. At all times, if the count is LT
   * zero, the function evaluates to false. The function evaluates to true
   * iff, the end left count is zero when the char list is empty.
   */
   def balance(chars: List[Char]): Boolean = {
   
    def loop(left: Int, chars: List[Char]): Boolean = {
      
      if (left < 0)
        false
      else
	      if (chars.isEmpty)
	       left == 0
	      else
	        if (chars.head == '(')
	          loop(left + 1, chars.tail)
	        else
	          if (chars.head == ')')
	            loop(left - 1, chars.tail)
	          else
	            loop(left, chars.tail)
    }
    
    loop(0, chars)
    
   }                                              //> balance: (chars: List[Char])Boolean
   
   balance(":-)".toList)                          //> res14: Boolean = false
   balance("())(".toList)                         //> res15: Boolean = false
   balance("((()()".toList)                       //> res16: Boolean = false
   balance(")(".toList)                           //> res17: Boolean = false
   
   balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList)
                                                  //> res18: Boolean = true
   balance("(if (zero? x) max (/1 x))".toList)    //> res19: Boolean = true
   balance("()".toList)                           //> res20: Boolean = true
   
   /**
    * Exercise 3 - Counting Change
    */
    def countChange(money: Int, coins: List[Int]): Int = {
    
      if (money == 0)
        // done
        1
      else if ((money < 0) || (money >= 1 && coins.isEmpty))
        // overshot or not enough coins
        0
      else
        // don't use current coin + use current coin, allow use again
        countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }                                             //> countChange: (money: Int, coins: List[Int])Int
    
    countChange(-1, List(1,2,3,4,5))              //> res21: Int = 0
    countChange(-1, List())                       //> res22: Int = 0
    countChange(1, List(2))                       //> res23: Int = 0
    
    countChange(0, List())                        //> res24: Int = 1
    countChange(1, List(1))                       //> res25: Int = 1
    countChange(1, List(1, 2))                    //> res26: Int = 1
    countChange(2, List(2))                       //> res27: Int = 1
    countChange(1, List(1,2,3,4,5))               //> res28: Int = 1
    
    countChange(2, List(1, 2))                    //> res29: Int = 2
    countChange(3, List(1, 2))                    //> res30: Int = 2
    countChange(4, List(1, 2))                    //> res31: Int = 3
    countChange(4, List(2, 1))                    //> res32: Int = 3
    
    countChange(10, List(1, 2, 5, 10))            //> res33: Int = 11
    countChange(10, List(5, 10, 2, 1))            //> res34: Int = 11
}