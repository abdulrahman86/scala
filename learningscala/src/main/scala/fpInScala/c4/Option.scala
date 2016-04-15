package fpInScala.c4

import java.util.regex.{Pattern, PatternSyntaxException}

trait Option[+A] {
  def map[B](f: A => B): Option[B] = None
  def flatMap[B](f: A => Option[B]): Option[B] = None
  def getOrElse[B >: A](default: => B): B = default
  def orElse[B >: A](ob: => Option[B]): Option[B] = None
  def filter(f: A => Boolean): Option[A] = None
}

final case object None extends Option[Nothing];

final case class Some[A](value: A) extends Option[A] {
  override def map[B](f: A => B): Option[B] =  Some(f(value))
  override def flatMap[B](f: A => Option[B]): Option[B] = f(value)
  override def getOrElse[B >: A](default: => B): B = value
  override def orElse[B >: A](ob: => Option[B]): Option[B] = this
  override def filter(f: A => Boolean): Option[A] = flatMap(x => if (f(x)) Some(x) else None)
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
