package fpInScala.c3

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by asattar on 2016-03-28.
  */

class LoopsSpec extends FlatSpec with Matchers {

  "A sum of List(1, 2, 3)" should "6" in {
    List.sum(List(1, 2, 3)) should be (6)
  }

}