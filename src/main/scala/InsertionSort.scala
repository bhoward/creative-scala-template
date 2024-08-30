def insert(nums: List[Int], n: Int): List[Int] = {
  nums match
    case Nil => List(n)
    case head :: tail =>
      if n < head then
        n :: nums
      else if n == head then
        nums
      else
        head :: insert(tail, n)
}

def insertion_sort(nums: List[Int]): List[Int] = {
  nums match
    case Nil => Nil
    case head :: tail => insert(insertion_sort(tail), head)
}

def isSorted(nums: List[Int]): Boolean = {
  nums match
    case Nil => true
    case _ :: Nil => true
    case a :: b :: rest => (a <= b) && isSorted(b :: rest)
}