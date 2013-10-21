package week03

object nth
{
  def nth[T](n: Int, xs: List[T]): T =
    if (n == 0) xs.head
    else nth(n - 1, xs.tail)                      //> nth: [T](n: Int, xs: week03.List[T])T
    
  val list = new Cons(1, new Cons(2, new Cons(3, new Nil)))
                                                  //> list  : week03.Cons[Int] = week03.Cons@22935862
  
  nth(2, list)                                    //> res0: Int = 3
}