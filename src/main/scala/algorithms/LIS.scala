package algorithms

object version1LIS {
  // Original recursive version: worst-case exponential time
  def version1(list: List[Int], start: Int = Int.MinValue): Int = {
    list match
      case Nil => 0
      case head :: tail =>
        if head > start
        then version1(tail, start) max (version1(tail, head) + 1)
        else version1(tail, start)
  }

  // Use array indices as arguments to aux function
  def version2(array: Array[Int]): Int = {
    val N = array.length

    def aux(list: Int, start: Int): Int =
      if list == N
      then 0
      else if start == N || array(list) > array(start)
      then aux(list + 1, start) max (aux(list + 1, list) + 1)
      else aux(list + 1, start)

    aux(0, N)
  }

  // Same as version2, plus a cache: worst-case quadratic time
  def version3(array: Array[Int]): Int = {
    val N = array.length
    val cache = scala.collection.mutable.Map[(Int, Int), Int]()

    def aux(list: Int, start: Int): Int =
      if cache.contains((list, start))
      then cache((list, start))
      else {
        val result =
          if list == N
          then 0
          else if start == N || array(list) > array(start)
          then aux(list + 1, start) max (aux(list + 1, list) + 1)
          else aux(list + 1, start)
        cache((list, start)) = result
        result
      }

    aux(0, N)
  }

  // Fill in version3 cache from the end: dynamic programming!
  def version4(array: Array[Int]): Int = {
    val N = array.length
    val cache = Array.ofDim[Int](N + 1, N + 1)

    for
      list <- N to 0 by -1
      start <- 0 to N
    do
      cache(list)(start) =
        if list == N
        then 0
        else if start == N || array(list) > array(start)
        then cache(list + 1)(start) max (cache(list + 1)(list) + 1)
        else cache(list + 1)(start)
    
    cache(0)(N)
  }

  @main
  def lisDemo(): Unit = {
    val input = List(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8)
    println(s"v1(12): ${version1(input)}")
    println(s"v2(12): ${version2(input.toArray)}")
    println(s"v3(12): ${version3(input.toArray)}")
    println(s"v4(12): ${version4(input.toArray)}")

    val n1 = 100
    val longInput = Seq.fill(n1)(scala.util.Random.nextInt)
    println(s"time v1($n1): ${time(version1(longInput.toList))}")
    println(s"time v2($n1): ${time(version2(longInput.toArray))}")
    println(s"time v3($n1): ${time(version3(longInput.toArray))}")
    println(s"time v4($n1): ${time(version4(longInput.toArray))}")

    val n2 = 120
    val longInput2 = Seq.fill(n2)(scala.util.Random.nextInt)
    println(s"time v1($n2): ${time(version1(longInput2.toList))}")
    println(s"time v2($n2): ${time(version2(longInput2.toArray))}")
    println(s"time v3($n2): ${time(version3(longInput2.toArray))}")
    println(s"time v4($n2): ${time(version4(longInput2.toArray))}")
  }


  def time(expr: => Int): (Int, Long) = {
    val start = System.nanoTime()
    val result = expr
    val stop = System.nanoTime()
    (result, stop - start)
  }
}
