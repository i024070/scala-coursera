package quickcheck

import java.util.NoSuchElementException

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("gen2") = forAll { (h: H) =>
    val k:Int = 1
    !isEmpty(insert(k, h))
  }

  property("gen3") = forAll { (h: H) =>
    val k:Int = 1
    !isEmpty(insert(k, h))
  }

  property("gen4") = forAll { (h: H) =>
    if (!isEmpty(h)) {
      val min = findMin(h)
      findMin(insert(min + 1, h)) == min
    } else {
      findMin(insert(1, h)) == 1
    }
  }
//
//  property("gen5") = forAll { (h: H) =>
//    if (!isEmpty(h)) {
//      val min = findMin(h)
//      findMin(insert(min + 1, h)) == min
//    } else {
//      try {
//        val min = findMin(h)
//        false
//      } catch {
//        case ex: NoSuchElementException => true
//      }
//    }
//  }
}
