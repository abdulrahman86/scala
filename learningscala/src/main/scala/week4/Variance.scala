package week4

/**
  * Created by asattar on 2016-03-27.
  */

class A[+T, -U, H] {

}

class B[-T]

class D[+T]

class C[+T] {

  def temp (x :B[T]) : D[T] =
    throw new Error("hi")
}

class BSuper {

}

class BDerived1 extends BSuper {

}

class BDerived2 extends BSuper {

}
