package smaass.peuler

import scala.annotation.tailrec

object P58 {
  
  def getCorners(last: Int, i: Int): Stream[Pair[Int, Int]] = {
    Stream((i, last + i), (i, last + 2*i)) append getCorners(last+2*i, i+1)
  }
  
  val corners = (2,3) #:: getCorners(3, 2)
  
  def conditionSide = {
    val iter = corners.iterator
    
    @tailrec
    def countPrimesRec(n: Int, p: Int): Int = {
      val next = iter.next
      if (p != 0 && n / p >= 10) return next._1
      else if (isPrime(next._2)) countPrimesRec(n+1, p+1)
      else countPrimesRec(n+1, p)
    }
    
    countPrimesRec(0, 0)
  }
  
  def main(args: Array[String]) = println("Solution: " + conditionSide)
}