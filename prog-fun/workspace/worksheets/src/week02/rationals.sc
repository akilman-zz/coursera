package week02

object rationals
{
  val x = new Rational(1,3)                       //> x  : week02.Rational = 1/3
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  
  val y = new Rational(5,7)                       //> y  : week02.Rational = 5/7
  x + y                                           //> res2: week02.Rational = 22/21
  
  val z = new Rational(3,2)                       //> z  : week02.Rational = 3/2
  
  val a = y - x                                   //> a  : week02.Rational = 8/21
  
  x - y - z                                       //> res3: week02.Rational = -79/42
  
  y + y                                           //> res4: week02.Rational = 10/7
  
  x < y                                           //> res5: Boolean = true
  x.max(y)                                        //> res6: week02.Rational = 5/7
  -x                                              //> res7: week02.Rational = 1/-3
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