package smaass.peuler

object P57 {
  
  class Rational(val n: BigInt, val d: BigInt) {
    val gcd = n.gcd(d)
    val num: BigInt = n/gcd
    val den: BigInt = d/gcd
    def + (that: Rational) = Rational(this.num*that.den + that.num*this.den, this.den * that.den)
    override def toString = num + "/" + den
  }
  object Rational {
    
    def apply(num: Int, den: Int) = new Rational(num, den)
    
    def apply(num: BigInt, den: BigInt) = new Rational(num, den)
    
    def apply(num: BigInt, den: Rational) = new Rational(num * den.den, den.num)
    
    implicit def intToRational(n: Int): Rational = new Rational(n, 1)
  }
  
  def getExpansion(n: Int): Rational = {
    def expansion(n: Int): Rational = {
      if (n > 0) Rational(1, 2 + expansion(n - 1))
      else 0
    }
    1 + expansion(n)
  }
  
  def testCondition(r: Rational) = digits(r.num) > digits(r.den)
  
  def digits(n: BigInt) = {
    def digitCount(num: BigInt, count: Int): Int = {
      val n: BigInt = num/10
      if (n > 0) digitCount(n, count+1)
      else count+1
    }
    digitCount(n, 0)
  }
  
  def main(args: Array[String]) = {
    def ocurrences(n: Int, c: Int): Int = {
      if (n == 0) c
      else if (testCondition(getExpansion(n))) ocurrences(n-1, c+1)
      else ocurrences(n-1, c)
    }
    val sol = ocurrences(1000, 0)
    println(s"Solution: $sol")
  }
}