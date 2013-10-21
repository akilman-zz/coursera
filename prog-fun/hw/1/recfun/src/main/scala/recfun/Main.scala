package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if (c < 0 || r < 0)
      0
    else if (c == 0 || r == c)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def loop(left: Int, chars: List[Char]): Boolean = {

      if (left < 0)
        false
      else if (chars.isEmpty)
        left == 0
      else if (chars.head == '(')
        loop(left + 1, chars.tail)
      else if (chars.head == ')')
        loop(left - 1, chars.tail)
      else
        loop(left, chars.tail)
    }

    loop(0, chars)

  }

  /**
   * Exercise 3
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
  }
}
