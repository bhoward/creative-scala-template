import org.scalatest.flatspec._
import org.scalatest.matchers.should._

class InsertionSortSpec extends AnyFlatSpec with Matchers:
  "Insertion Sort" should "sort a list of integers in ascending order" in {
    val unsortedList = List(4, 6, 1, 3, 5)
    val sortedList   = List(1, 3, 4, 5, 6)
    insertion_sort(unsortedList) shouldEqual sortedList
  }

  it should "handle an empty list" in {
    insertion_sort(Nil) shouldEqual Nil
  }

  it should "handle a list with a single element" in {
    val singleElementList = List(42)
    insertion_sort(singleElementList) shouldEqual singleElementList
  }

  it should "handle a list with duplicate elements" in {
    val listWithDuplicates = List(3, 1, 4, 1, 5, 9, 2, 6, 5)
    val sortedlist         = List(1, 1, 2, 3, 4, 5, 5, 6, 9)
    insertion_sort(listWithDuplicates) shouldEqual sortedlist
  }
