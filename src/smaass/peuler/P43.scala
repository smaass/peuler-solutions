package smaass.peuler

object P43 {
  
  val pandigitals = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).permutations
  val primes = List(2, 3, 5, 7, 11, 13, 17)
  
  def getNum(l: List[Int]): Long = l.reverse.zipWithIndex.map(p => p._1 * Math.pow(10, p._2)).reduce(_ + _).asInstanceOf[Long]
  
  def testProperty(l: List[Int]): Boolean = {
    val slices = (1 to 7).map(i => getNum(l.slice(i, i+3)))
    slices.zipWithIndex.map(p => p._1 % primes(p._2) == 0).reduce(_ && _)
  }
  
  def solve: Long = {
    def solutionList(acc: List[Long]): List[Long] = {
      if (pandigitals.hasNext) {
        val next = pandigitals.next
        if (testProperty(next)) solutionList(getNum(next) :: acc)
        else solutionList(acc)
      }
      else acc
    }
    solutionList(List()).reduce(_ + _)
  }
  
  def main(args: Array[String]) {
    println("Solution: " + solve)
  }
}