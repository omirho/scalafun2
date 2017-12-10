package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      k <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(k, h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    (findMin(h) == a.min(b)) &&
    (findMin(deleteMin(h)) == a.max(b))
  }

  property("del1") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  def sorted(h: H): (Boolean, List[Int]) = {
    if (isEmpty(h)) (true, Nil)
    else {
      val m = findMin(h)
      val (a, b) = sorted(deleteMin(h))
      (a && (b match {
        case Nil => true
        case x :: _ => m <= x
      }), m :: b)
    }
  }

  property("sorted") = forAll { (h: H) =>
    sorted(h)._1
  }

  property("meldMin") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val m = if (isEmpty(h)) 0 else findMin(h)
    val m1 = if (isEmpty(h1)) 0 else findMin(h1)
    val m2 = if (isEmpty(h2)) 0 else findMin(h2)
    (m == m1) || (m == m2)
  }

  property("gen2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    Set(a, b) == Set(findMin(h), findMin(deleteMin(h)))
  }

  property("gen3") = forAll { a: List[Int] =>
    val h = (a foldLeft empty)((acc, b) => insert(b, acc))
    sorted(h)._2 == a.sorted
  }
}
