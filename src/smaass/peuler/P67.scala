package smaass.peuler

import scala.io.Source

object P67 {
  val triangle: List[List[Int]] =
    Source.fromFile("triangle.txt").getLines.toList.map(l => l.split(" ").toList.map(s => s.toInt))
    
  def mixRow(big: List[Int], small: List[Int]): List[Int] = (big, small) match {
    case (bf :: bs :: brest, sf :: srest) => sf + Math.max(bf, bs) :: mixRow(bs :: brest, srest)
    case (bf :: brest, s) if (s.length == 1) => List(s.head + Math.max(bf, brest.head))
    case (b, Nil) => Nil
  }
  
  def solve = triangle.reverse.reduce((b, s) => mixRow(b, s)).head
  
  def main(args: Array[String]) = println("Solution: " + solve)
}