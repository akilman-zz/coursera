package streams

object temp {
  val myList = "abcdef".toList                    //> myList  : List[Char] = List(a, b, c, d, e, f)
  'z' :: myList                                   //> res0: List[Char] = List(z, a, b, c, d, e, f)
}