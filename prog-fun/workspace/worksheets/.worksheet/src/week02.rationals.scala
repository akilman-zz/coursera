package week02

object rationals
{;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(62); 
  val x = new Rational(1,3);System.out.println("""x  : week02.Rational = """ + $show(x ));$skip(10); val res$0 = 
  x.numer;System.out.println("""res0: Int = """ + $show(res$0));$skip(10); val res$1 = 
  x.denom;System.out.println("""res1: Int = """ + $show(res$1));$skip(31); 
  
  val y = new Rational(5,7);System.out.println("""y  : week02.Rational = """ + $show(y ));$skip(8); val res$2 = 
  x + y;System.out.println("""res2: week02.Rational = """ + $show(res$2));$skip(31); 
  
  val z = new Rational(3,2);System.out.println("""z  : week02.Rational = """ + $show(z ));$skip(19); 
  
  val a = y - x;System.out.println("""a  : week02.Rational = """ + $show(a ));$skip(15); val res$3 = 
  
  x - y - z;System.out.println("""res3: week02.Rational = """ + $show(res$3));$skip(11); val res$4 = 
  
  y + y;System.out.println("""res4: week02.Rational = """ + $show(res$4));$skip(11); val res$5 = 
  
  x < y;System.out.println("""res5: Boolean = """ + $show(res$5));$skip(11); val res$6 = 
  x.max(y);System.out.println("""res6: week02.Rational = """ + $show(res$6));$skip(5); val res$7 = 
  -x;System.out.println("""res7: week02.Rational = """ + $show(res$7))}
}

class Rational(x: Int, y: Int)
{
  require(y != 0, "Denominator must be non-zero")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  def numer = x
  def denom = y
  
  def < (that: Rational) = numer * that.denom < that.numer * denom
  
  def max(that: Rational) = if (this < that) that else this
  
  def + (that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
      
  def - (that: Rational) = this + -that
      
  def unary_- : Rational = new Rational(-numer, denom)
  
  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}
