package demo

import org.scalatest.FunSuite

/**
  * Created by asattar on 2016-03-25.
  */
class HelloTest extends FunSuite {

  test("Hello method works correctly") {
    val hello = new Hello
    assert(hello.sayHello("Scala").equals("Hello Scala"))
  }
}
