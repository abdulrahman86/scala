package fpInScala.c3

/**
  * Created by asattar on 2016-04-13.
  */
trait MyTrait[T] {

  outer =>

  def someMethod(a: T, b: T) : T
}

//class MyClass {
//
//  implicit def convert: MyClass => MyTrait[MyClass] = { f =>
//    new MyTrait[MyClass] {
//      override def someMethod(a: MyClass) = a
//    }
//  }
//}


//trait MyClassTrait extends MyTrait[MyClass] {
//  override  def someMethod(a: MyClass) = a
//}

object Test {
  def someOtherMethod[A : MyTrait] (a: A, b: A)= {
    val myTrait = implicitly[MyTrait[A]]
    myTrait.someMethod(a, b)
  }
}
