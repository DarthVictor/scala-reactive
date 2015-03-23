package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

    property("min1") = forAll { a: Int =>
        val h = insert(a, empty)
        findMin(h) == a
    }

    property("gen1") = forAll { (h: H) =>
        val m = if (isEmpty(h)) 0 else findMin(h)
        findMin(insert(m, h)) == m
    } 
    
    lazy val genHeap: Gen[H] = for{
        v <- arbitrary[Int]
        heap <- oneOf(value(empty), genHeap)
    } yield insert(v, heap)

    lazy val genMap: Gen[Map[Int,Int]] = for {
        k <- arbitrary[Int]
        v <- arbitrary[Int]
        m <- oneOf(value(Map.empty[Int,Int]), genMap)
    } yield m.updated(k, v)
  
    implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
  
    property("hint1") = forAll { (a: Int, b: Int) =>
        val h = insert(b, insert(a, empty))
        findMin(h) == (if (a > b) b else a)
    }
    
    property("hint2") = forAll {  a: Int =>
        val h = deleteMin(insert(a, empty))
        isEmpty(h)
    }
    
    property("hint3") = forAll { (h: H) =>
        
        def propertyHelper(heap: H, previousMin: Int):Boolean = {
                if (isEmpty(heap)) true 
                else {
                    val currentMin = findMin(heap);
                    (currentMin >= previousMin) && propertyHelper(deleteMin(heap), currentMin)
                }                
        }
        val firstMin = findMin(h);
        propertyHelper(deleteMin(h), firstMin)
    } 
}
