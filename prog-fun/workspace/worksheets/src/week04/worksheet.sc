package week04

object worksheet {
  val l = List('a', 'a', 'a', 'a', 'b', 'b', 'c', 'c', 'c')
                                                  //> l  : List[Char] = List(a, a, a, a, b, b, c, c, c)
  println(l.groupBy(x => x).mapValues(v => v.length).toList.sortBy(p => -p._2))
                                                  //> List((a,4), (c,3), (b,2))
}