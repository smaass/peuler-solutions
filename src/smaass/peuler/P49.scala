package smaass.peuler

object P49 {
  
  val primes = getPrimes.dropWhile(p => p < 1000).takeWhile(p => p < 10000).toList
  
  def arePermutations(nums: List[Int]): Boolean = {
    if (nums.length == 0) throw new Exception("Empty list!")
    if (nums.length == 1) true
    val f = intToList(nums.head)
    val it = nums.tail.iterator
    def checkPermutations: Boolean = {
      if (!it.hasNext) true
      else if (arePermutations(f, intToList(it.next))) checkPermutations
      else false
    }
    checkPermutations
  }
  
  def arePermutations[A](l1: List[A], l2: List[A]): Boolean = {
    if (l1.length != l2.length) false
    
    (for (i <- 0 until l1.length) yield {
      l1.count(p => p == l1(i)) == l2.count(p => p == l1(i))
    }).reduce(_ && _)
  }
  
  def main(args: Array[String]) = {
    for (num <- primes; inc <- 1 to (10000-num)/2) {
      val num2 = num + inc
      val num3 = num + 2*inc
      if (primes.contains(num2) && primes.contains(num3) && arePermutations(List(num, num2, num3)))
        println(s"$num $num2 $num3")
    }
  }
}