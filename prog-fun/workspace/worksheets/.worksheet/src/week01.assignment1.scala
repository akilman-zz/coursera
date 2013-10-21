package week01

object assignment1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(268); 

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
  };System.out.println("""pascal: (c: Int, r: Int)Int""");$skip(40); val res$0 = 
  
  // negative cases
  pascal(-1, -1);System.out.println("""res0: Int = """ + $show(res$0));$skip(16); val res$1 = 
  pascal(-1, 0);System.out.println("""res1: Int = """ + $show(res$1));$skip(16); val res$2 = 
  pascal(0, -1);System.out.println("""res2: Int = """ + $show(res$2));$skip(38); val res$3 = 
  
  // positive cases
  pascal(0, 0);System.out.println("""res3: Int = """ + $show(res$3));$skip(15); val res$4 = 
  pascal(0, 1);System.out.println("""res4: Int = """ + $show(res$4));$skip(15); val res$5 = 
  pascal(0, 2);System.out.println("""res5: Int = """ + $show(res$5));$skip(15); val res$6 = 
  pascal(0, 3);System.out.println("""res6: Int = """ + $show(res$6));$skip(15); val res$7 = 
  pascal(1, 0);System.out.println("""res7: Int = """ + $show(res$7));$skip(15); val res$8 = 
  pascal(2, 0);System.out.println("""res8: Int = """ + $show(res$8));$skip(15); val res$9 = 
  pascal(3, 0);System.out.println("""res9: Int = """ + $show(res$9));$skip(53); val res$10 = 
  
  // cited in exercise description
  pascal(0, 2);System.out.println("""res10: Int = """ + $show(res$10));$skip(15); val res$11 = 
  pascal(1, 2);System.out.println("""res11: Int = """ + $show(res$11));$skip(15); val res$12 = 
  pascal(1, 3);System.out.println("""res12: Int = """ + $show(res$12));$skip(15); val res$13 = 
  pascal(2, 4);System.out.println("""res13: Int = """ + $show(res$13));$skip(813); 
  
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
    
   };System.out.println("""balance: (chars: List[Char])Boolean""");$skip(29); val res$14 = 
   
   balance(":-)".toList);System.out.println("""res14: Boolean = """ + $show(res$14));$skip(26); val res$15 = 
   balance("())(".toList);System.out.println("""res15: Boolean = """ + $show(res$15));$skip(28); val res$16 = 
   balance("((()()".toList);System.out.println("""res16: Boolean = """ + $show(res$16));$skip(24); val res$17 = 
   balance(")(".toList);System.out.println("""res17: Boolean = """ + $show(res$17));$skip(90); val res$18 = 
   
   balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList);System.out.println("""res18: Boolean = """ + $show(res$18));$skip(47); val res$19 = 
   balance("(if (zero? x) max (/1 x))".toList);System.out.println("""res19: Boolean = """ + $show(res$19));$skip(24); val res$20 = 
   balance("()".toList);System.out.println("""res20: Boolean = """ + $show(res$20));$skip(443); 
   
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
    };System.out.println("""countChange: (money: Int, coins: List[Int])Int""");$skip(42); val res$21 = 
    
    countChange(-1, List(1,2,3,4,5));System.out.println("""res21: Int = """ + $show(res$21));$skip(28); val res$22 = 
    countChange(-1, List());System.out.println("""res22: Int = """ + $show(res$22));$skip(28); val res$23 = 
    countChange(1, List(2));System.out.println("""res23: Int = """ + $show(res$23));$skip(32); val res$24 = 
    
    countChange(0, List());System.out.println("""res24: Int = """ + $show(res$24));$skip(28); val res$25 = 
    countChange(1, List(1));System.out.println("""res25: Int = """ + $show(res$25));$skip(31); val res$26 = 
    countChange(1, List(1, 2));System.out.println("""res26: Int = """ + $show(res$26));$skip(28); val res$27 = 
    countChange(2, List(2));System.out.println("""res27: Int = """ + $show(res$27));$skip(36); val res$28 = 
    countChange(1, List(1,2,3,4,5));System.out.println("""res28: Int = """ + $show(res$28));$skip(36); val res$29 = 
    
    countChange(2, List(1, 2));System.out.println("""res29: Int = """ + $show(res$29));$skip(31); val res$30 = 
    countChange(3, List(1, 2));System.out.println("""res30: Int = """ + $show(res$30));$skip(31); val res$31 = 
    countChange(4, List(1, 2));System.out.println("""res31: Int = """ + $show(res$31));$skip(31); val res$32 = 
    countChange(4, List(2, 1));System.out.println("""res32: Int = """ + $show(res$32));$skip(44); val res$33 = 
    
    countChange(10, List(1, 2, 5, 10));System.out.println("""res33: Int = """ + $show(res$33));$skip(39); val res$34 = 
    countChange(10, List(5, 10, 2, 1));System.out.println("""res34: Int = """ + $show(res$34))}
}
