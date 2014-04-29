package smaass.peuler

import scala.collection.immutable.Stream.consWrapper
import scala.math.BigInt.int2bigInt

object P104 {
  
  import Math._
  
  val logphi = log10((1 + sqrt(5))/2)
  val logsr5 = log10(sqrt(5))
  
  def findN = {
    var done = false
    var fn: Long = 0
    var fn1: Long = 1
    var fn2: Long = 1
    var n = 2
    while (!done) {
      n += 1
      fn = (fn1 + fn2) % 1000000000
      fn2 = fn1
      fn1 = fn
      if (isPandigital(fn, 9)) {
        var m = n * logphi - logsr5
        if (isPandigital(pow(10, (m - floor(m) + 8)).toLong, 9)) done = true
      }
    }
    n
  }
  
  def main(args: Array[String]) = {
    println("Solution: " + findN)
  }
}