package smaass.peuler

import scala.collection.mutable.LinkedList

object P87 {

  val primes: Array[Int] = getPrimes.takeWhile(p => p <= 7071).toArray // 7071^2 = 49999041
  
  def numbers = {
    var store = new Array[Int](1140000) // Upper limit determined experimentally by running without removing duplicates
    var count = 0
    var i: Int = 0
    var j: Int = 0
    var k: Int = 0
    var nextN: Int = primeFactSum(primes(i), primes(j), primes(k));
    while (nextN < 50000000 && i < primes.size) {
      while (nextN < 50000000 && j < primes.size) {
        while (nextN < 50000000 && k < primes.size) {
          store.update(count, nextN)
          count += 1
          k += 1
          if (i < primes.size) nextN = primeFactSum(primes(i), primes(j), primes(k))
        }
        k = 0
        j += 1
        if (i < primes.size) nextN = primeFactSum(primes(i), primes(j), primes(k))
      }
      j = 0
      i += 1
      if (i < primes.size) nextN = primeFactSum(primes(i), primes(j), primes(k))
    }
    store.distinct.filter(n => n != 0).size
  }
  
  def primeFactSum(square: Int, cube: Int, fourth: Int) =
    square*square + cube*cube*cube + fourth*fourth*fourth*fourth
  
  def main(args: Array[String]) = {
    println("Solution: " + numbers)
  }
  
}