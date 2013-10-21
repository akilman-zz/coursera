package funsets

object Main extends App {
  
  import FunSets._
  
  final val perfectSquareSet = map((x: Int) => x > 0, (x: Int) => x * x)
//  println(contains(perfectSquareSet, 2))
  
  def s = (x: Int) => f(x) > 0
  def f = (x: Int) => x * x
  
  // {1 2, 3, 4, ..., n}
  val N = 10
  def oneToN = (x: Int) => x < 0
  
  // {1, 4, 9 16, ..., n^2}
  def squares = range(10, 20)
  printSet(squares)
  
}