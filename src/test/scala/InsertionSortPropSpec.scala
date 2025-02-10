import org.scalacheck.*

object InsertionSortPropSpec extends Properties("Insertion Sort") {
  import Prop.forAll

  property("same length") = forAll { (a: List[Int]) =>
    a.length == insertion_sort(a).length
  }

  property("sorted") = forAll { (a: List[Int]) =>
    isSorted(insertion_sort(a))
  }

  property("same elements") = forAll { (a: List[Int]) =>
    val aSorted = insertion_sort(a)
    a.forall(x => aSorted contains x) && aSorted.forall(x => a contains x)
  }
}

