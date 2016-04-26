package fpInScala.c3

/**
  * Created by asattar on 2016-04-13.
  */
trait MyTrait[T] {

  outer =>

  def someMethod(a: T, b: T) : T
}

trait Functor[F[_]]{
  def fmap[A, B](r: F[A], f: A => B): F[B]
}

object Functor {

  implicit def Tuple1Functor: Functor[Tuple1] = new Functor[Tuple1] {
    def fmap[A, B](r: Tuple1[A], f: A => B) = Tuple1(f(r._1))
  }
}


object Test {
  def someOtherMethod[A : MyTrait] (a: A, b: A)= {
    val myTrait = implicitly[MyTrait[A]]
    myTrait.someMethod(a, b)
  }

  def test[A, B](a: Tuple1[A])(op : A => B): Tuple1[B] = {
    val functor = implicitly[Functor[Tuple1]]
    functor.fmap(a, op)
  }

  def test2[A[_] : Functor, C, D](a: A[C])(f : C => D) =
    implicitly[Functor[A]].fmap[C, D](a, f)
}
