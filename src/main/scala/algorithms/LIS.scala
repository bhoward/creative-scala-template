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
    println(version1(input))
    println(version2(input.toArray))
    println(version3(input.toArray))
    println(version4(input.toArray))

    val longInput = Seq.fill(100)(scala.util.Random.nextInt)
    println(time(version1(longInput.toList)))
    println(time(version2(longInput.toArray)))
    println(time(version3(longInput.toArray)))
    println(time(version4(longInput.toArray)))

    val longInput2 = Seq.fill(120)(scala.util.Random.nextInt)
    println(time(version1(longInput2.toList)))
    println(time(version2(longInput2.toArray)))
    println(time(version3(longInput2.toArray)))
    println(time(version4(longInput2.toArray)))
  }


  def time(expr: => Int): (Int, Long) = {
    val start = System.nanoTime()
    val result = expr
    val stop = System.nanoTime()
    (result, stop - start)
  }
}
