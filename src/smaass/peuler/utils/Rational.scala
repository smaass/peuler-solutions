package smaass.peuler.utils

class Rational(val n: BigInt, val d: BigInt) {
  require(d != 0)
  val gcd = n.gcd(d)
  val num: BigInt = if (d > 0) n/gcd else -n/gcd
  val den: BigInt = if (d > 0) d/gcd else -d/gcd
  
  def + (that: Rational) = Rational(num * that.den + that.num * den, den * that.den)
  def - (that: Rational) = Rational(num * that.den - that.num * den, den * that.den)
  def * (that: Rational) = Rational(num * that.num, den * that.den)
  def / (that: Rational) = Rational(num * that.den, den * that.num)
  
  override def toString = num + "/" + den
}

object Rational {
  def apply(num: Int, den: Int) = new Rational(num, den)
  def apply(num: BigInt, den: BigInt) = new Rational(num, den)
  def apply(num: BigInt, den: Rational) = new Rational(num * den.den, den.num)
  implicit def intToRational(n: Int): Rational = new Rational(n, 1)
}