import org.scalatest.flatspec._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Prop.forAll

class InsertionSortPropSpec extends AnyFlatSpec:
  "Insertion Sort" should "preserve the list length" in {
    forAll { (a: List[Int]) =>
      a.length == insertion_sort(a).length
    }
  }

  it should "produce a sorted list" in {
    forAll { (a: List[Int]) =>
      isSorted(insertion_sort(a))
    }
  }

  it should "keep the same elements" in {
    forAll { (a: List[Int]) =>
      val aSorted = insertion_sort(a)
      a.forall(x => aSorted contains x) && aSorted.forall(x => a contains x)
    }
  }
