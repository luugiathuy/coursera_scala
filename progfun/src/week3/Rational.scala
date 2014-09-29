package week3

class Rational(x: Int, y: Int) {
	require(y != 0, "denominator mus be non-zero")
	
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	private val g = gcd(x, y)
  val numer = x / g
  val denom = y / g
  
  def < (that: Rational) = numer * that.denom < that.numer * denom
  
  def max(that: Rational) = if (this < that) that else this

  def +(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom)
      
	def unary_- = new Rational(-numer, denom)
	
	def - (that: Rational) = this + -that
      
  override def toString = numer + "/" + denom
}