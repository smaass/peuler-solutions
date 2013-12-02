package smaass.peuler

import scala.annotation.tailrec

object Utils {
  
  def getPrimes = {
    def sieve(s: Stream[Int]): Stream[Int] = {
      s.head #:: sieve(s.tail.filter(_ % s.head != 0))
    }
    sieve(Stream.from(2))
  }
  
  def intToList(n: Int): List[Int] = {
    @tailrec
    def carryDigits(n: Int, carry: List[Int]): List[Int] = {
      if (n < 10) n :: carry
      else carryDigits(n/10, (n % 10) :: carry)
    }
    carryDigits(n, List())
  }
  
}