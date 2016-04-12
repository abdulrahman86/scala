package fpInScala.c3

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by asattar on 2016-03-28.
  */

class LoopsSpec extends FlatSpec with Matchers {

  "A sum of List(1, 2, 3)" should "6" in {
    List.sum(List(1, 2, 3)) should be (6)
  }

  "List(1, 2, 3) map x=> x+1" should "List(2, 3, 4)" in {
    List.map(List(1, 2, 3))(x => x+1 ) should be (List(2, 3, 4))
  }

  "List(1, 2, 3) map x=> List(x+1)" should "List(2, 3, 4)" in {
    List.flatMap(List(1, 2, 3))(x => List(x+1) ) should be (List(2, 3, 4))
  }

  "List(1, 2, 3) filter x=> x >2" should "List(3)" in {
    List.filter(List(1, 2, 3))(x => x > 2 ) should be (List(3))
  }

  "zipWith List(1, 2, 3) List(4, 5, 6)  _ + _" should "List(5, 7, 9)" in {
    List.zipWith(List(1, 2, 3))(List(4, 5, 6))(_ + _) should be (List(5, 7, 9))
  }
}