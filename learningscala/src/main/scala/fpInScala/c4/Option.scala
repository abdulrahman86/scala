package fpInScala.c4

import java.util.regex.{Pattern, PatternSyntaxException}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]): Option[B]
  def getOrElse[B >: A](default: => B): B
  def orElse[B >: A](ob: => Option[B]): Option[B]
  def filter(f: A => Boolean): Option[A]
}

final case object None extends Option[Nothing] {
  override def map[B](f: Nothing => B): Option[B] = None
  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None
  override def getOrElse[B >: Nothing](default: => B): B = default
  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = None
  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}

final case class Some[A](value: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] =  Some(f(value))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(value)
  override def getOrElse[B >: A](default: => B): B = value
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  override def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B]
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B]
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C):
  Either[EE, C]
}
case class Left[+E](value: E) extends Either[E, Nothing] {
  override def map[B](f: Nothing => B): Either[E, B] = this
  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this
  override def orElse[EE >: E,B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b
  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C) : Either[EE, C] = this
}
case class Right[+A](value: A) extends Either[Nothing, A] {
  override def map[B](f: A => B): Either[Nothing, B] = Right(f(value))
  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(value)
  override def orElse[EE >: Nothing,B >: A](b: => Either[EE, B]): Either[EE, B] = this
  override def map2[EE >: Nothing, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = b map(f(value, _))
}

object Option {

  def pattern(s: String): Option[Pattern] =
    try {
      Some(Pattern.compile(s))
    } catch {
      case e: PatternSyntaxException => None
    }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(x => mean(xs.map(y => Math.pow(y - x, 2))))

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (x => b map ( y=> f(x, y)))

  def bothMatch_2(pat1: String, pat2: String, s: String): Option[Boolean] =
    map2[Pattern, Pattern, Boolean](pattern(pat1), pattern(pat2))((p1, p2) => (p1 matcher(s) matches) && (p2 matcher(s) matches))

  def sequence[A](l: List[Option[A]]): Option[List[A]] =
    l.foldRight[Option[List[A]]](Some(Nil))((e: Option[A], l : Option[List[A]]) => l flatMap(list => e map(head => head :: list)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(List()))((h, t) => t flatMap  (tail => f(h) map(head => head :: tail)))

  def sequence2[A](l: List[Option[A]]): Option[List[A]] =
    traverse[Option[A], A](l)((x: Option[A]) => x)

  def of[A](value: A) : Option[A] = Some(value)
}

object Either {

  def Try[A](x : => A) : Either[Exception, A] =
    try Right(x)
    catch {case e : Exception => Left(e)}

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldLeft[Either[E,List[B]]](Right(Nil))((x, y) => x flatMap (arg1 => f(y) map (arg2 =>  (arg2 :: (arg1 reverse)) reverse)))

  def sequence[E, A] (es: List[Either[E, A]]) : Either[E, List[A]] =
    traverse(es)(x => x)

}
