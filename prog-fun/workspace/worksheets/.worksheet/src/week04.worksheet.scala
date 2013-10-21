package week04

object worksheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(94); 
  val l = List('a', 'a', 'a', 'a', 'b', 'b', 'c', 'c', 'c');System.out.println("""l  : List[Char] = """ + $show(l ));$skip(80); 
  println(l.groupBy(x => x).mapValues(v => v.length).toList.sortBy(p => -p._2))}
}
