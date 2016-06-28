package fpInScala.c6

object State {

  type State[S, +A] = S => (A, S)

  def unit[S, A] (a: A) : State[S, A] =
    (a , _)

  def map[S, A, B](f: A => B)(s: State[S, A]) : State[S, B] =
    t => {
      (f(s(t)._1), s(t)._2)
    }

  def map2[S, A, B, C](f: (A, B) => C)(s1: State[S, A])(s2: State[S, B]) : State[S, C] =
    s => {
      val (t1, y1) = s1(s)
      val (t2, y2) = s2(y1)
      (f(t1, t2), y2)
    }

  def flatMap[S, A, B](s1: State[S, A])(f: A => State[S, B]): State[S, B] =
    s => {
      val (t1, y1) = s1(s)
      f(t1)(y1)
    }

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight[State[S, List[A]]](unit(List()))((b, s) => {
      s1 => {
        val (t1, s2) = b(s1)
        val (t2, s3) = s(s2)
        (t1 :: t2, s3)
      }
    })

}
