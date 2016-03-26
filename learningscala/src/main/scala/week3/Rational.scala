package week3

/**
  * Created by asattar on 2016-03-25.
  */
class Rational (x: Int, y: Int = 1) {
  private def gcd(a: Int, b: Int) : Int = if (b == 0) a else gcd (b, a % b)

  def < (that:Rational) = this.numer * that.denom < that.numer * this.denom
  private val g = gcd(x, y)
  val numer = x / Math.abs(g)
  val denom = y / Math.abs(g)

  def unary_- = new Rational(-numer, denom)

  override def toString : String =
    numer + "/" + denom
}
