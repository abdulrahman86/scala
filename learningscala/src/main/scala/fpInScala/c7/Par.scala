package fpInScala.c7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}




object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: => A) : Par[A] = es => UnitFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]) : A = a(s).get()

  def map2[A, B, C](f: (A, B) => C)(a: Par[A])(b: Par[B]) : Par[C] =
    es => {
      val r1: Future[A] = a(es)
      val r2: Future[B] = b(es)
      UnitFuture(f(r1.get(), r2.get()))
    }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit (List()))((a: Par[A], b: Par[List[A]]) => {
      es: ExecutorService => {
        val r1 = a(es)
        val r2 = b(es)
        UnitFuture(r1.get() :: r2.get())
      }
    })

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs : List[Par[B]] = ps.map(async(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = es => {
    UnitFuture(parMap[A,List[A]](as)(x => {
      if (f(x)) List(x)
      else List()
    })(es).get().foldRight[List[A]](List())(_ ++ _))
  }


  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2[A,Int, B]((a, _ ) => f(a))(pa)(unit(1))

  def async[A, B](f: A => B): A => Par[B] =
    a => fork(unit(f(a)))

  def fork[A](a: Par[A]) : Par[A] = es => es.submit(new Callable[A] {
    override def call(): A = a(es).get()
  })

  def product[A, B] (fa: Par[A], fb: Par[B]): Par[(A, B)] =
    es => {
      val a = fa(es)
      val b = fb(es)
      UnitFuture((a.get(), b.get()))
    }

  def map1[A, B](pa: Par[A])(f: A => B): Par[B] =
    es => {
      UnitFuture(f(pa(es).get()))
    }

  def map21[A, B, C](f: ((A, B)) => C)(a: Par[A])(b: Par[B]) : Par[C] =
    map1[(A, B), C](product[A, B](a, b))(f)


  def lazyUnit[A](a: => A) : Par[A] = fork(unit(a))

  private case class UnitFuture[A] (get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }
}
