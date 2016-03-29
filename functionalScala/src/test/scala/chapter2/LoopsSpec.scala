package chapter2

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by asattar on 2016-03-28.
  */
import Loops2._

class LoopsSpec extends FlatSpec with Matchers {

  "fib" should "return 0" in {
    fibTailRecur(4) should be (3)
  }

  "findFirst" should "return 3" in {
    findFirst2[Int](Array(1, 2, 3), (x) => x ==2) should be (2)
  }

  "isSorted" should "return true" in {
    isSorted[Int](Array(1, 2, 3), (x, y) => x < y) should be (true)
  }

  "isSorted" should "return false" in {
    isSorted[Int](Array(1, 3, 2), (x, y) => x < y) should be (false)
  }
}