package smaass

import scala.annotation.tailrec

package object peuler {
  
  def getPrimes = {
    def sieve(s: Stream[Int]): Stream[Int] = {
      s.head #:: sieve(s.tail.filter(_ % s.head != 0))
    }
    sieve(Stream.from(2))
  }
  
  def isPrime(n: Int): Boolean = {
    val l: Int = Math.floor(Math.sqrt(n)).asInstanceOf[Int]
    @tailrec
    def isPrimeRec(div: Int): Boolean = {
      if (div > l) true
      else if (n % div == 0) false
      else isPrimeRec(div+1)
    }
    isPrimeRec(2)
  }
  
  implicit def intToList(n: Int): List[Int] = bigIntToList(n)
  
  implicit def longToList(n: Long): List[Int] = bigIntToList(n)
  
  implicit def bigIntToList(n: BigInt): List[Int] = bigIntToStream(n).toList
  
  implicit def bigIntToStream(n: BigInt): Stream[Int] = {
    @tailrec
    def carryDigits(n: BigInt, carry: Stream[Int]): Stream[Int] = {
      if (n < 10) n.toInt #:: carry
      else carryDigits(n/10, (n % 10).toInt #:: carry)
    }
    carryDigits(n, Stream())
  }
  
  def isPandigital(n: List[Int], digits: Int): Boolean = {
    if (n.map(x => if (x > digits || x == 0) true else false).reduce(_ || _)) false
    else {
      val has = new Array[Boolean](digits)
      for (d <- n) has(d-1) = true
      has.reduce(_ && _)
    }
  }
}