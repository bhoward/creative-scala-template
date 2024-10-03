package algorithms

// Based on J. Bentley, "Programming Pearls" (2nd Ed, 2000), Chapter 8
// Find the maximum sum over all contiguous subarrays of the input

// Naive cubic time solution
def maxSubarray1(a: Vector[Double]): Double = {
    var maxSoFar = 0.0
    for i <- 0 until a.size do
        for j <- i until a.size do
            var sum = 0.0
            for k <- i to j do
                sum = sum + a(k)
            maxSoFar = maxSoFar max sum
    maxSoFar
}

// Quadratic time solution reusing previous sums
def maxSubarray2(a: Vector[Double]): Double = {
    var maxSoFar = 0.0
    for i <- 0 until a.size do
        var sum = 0.0
        for j <- i until a.size do
            sum = sum + a(j)
            // now sum is sum of a(i..j)
            maxSoFar = maxSoFar max sum
    maxSoFar
}

// Quadratic time solution with precomputed partial sums
def maxSubarray2b(a: Vector[Double]): Double = {
    val cumulative = a.scanLeft(0.0)(_ + _)
    var maxSoFar = 0.0
    for i <- 0 until a.size do
        for j <- i until a.size do
            val sum = cumulative(j + 1) - cumulative(i)
            // now sum is sum of a(i..j)
            maxSoFar = maxSoFar max sum
    maxSoFar
}

// N log N time solution by divide and conquer
def maxSubarray3(a: Vector[Double]): Double = {
    def aux(lo: Int, hi: Int): Double = {
        if (lo > hi) then 0.0
        else if (lo == hi) then 0.0 max a(lo)
        else {
            val mid = lo + (hi - lo) / 2
            val left = aux(lo, mid)
            val right = aux(mid + 1, hi)

            // find maximum from mid to left
            var lmax = 0.0
            var lsum = 0.0
            for i <- mid to lo by -1 do
                lsum = lsum + a(i)
                lmax = lmax max lsum
            
            // find maximum from mid to right
            var rmax = 0.0
            var rsum = 0.0
            for i <- mid + 1 to hi do
                rsum = rsum + a(i)
                rmax = rmax max rsum

            (lmax + rmax) max left max right
        }
    }

    aux(0, a.size - 1)
}

def maxSubarray4(a: Vector[Double]): Double = {
    var maxSoFar = 0.0
    var maxEndingHere = 0.0
    for i <- 0 until a.size do
        maxEndingHere = (maxEndingHere + a(i)) max 0
        maxSoFar = maxSoFar max maxEndingHere
    maxSoFar
}

def time[T](body: => T): (Double, T) = {
    val start = System.nanoTime()
    val result = body
    val stop = System.nanoTime()
    ((stop - start) / 1e9, result)
}

@main def msa(n: Int): Unit = {
    val data = Vector.fill(n)(math.random() * 2 - 1)

    if (n <= 2000) then
        println("1: " + time(maxSubarray1(data)))
    else
        println("Skipping version 1")
    if (n <= 40000) then
        println("2: " + time(maxSubarray2(data)))
        println("2b: " + time(maxSubarray2b(data)))
    else
        println("Skipping version 2")
    println("3: " + time(maxSubarray3(data)))
    println("4: " + time(maxSubarray3(data)))
}
