package fpInScala.c5

object Lazy {

  def square(x: => Int) = {
    lazy val i = x
    i * i
  }
  def if2[A](cond: Boolean, onTrue : => A, onFalse : => A) =
    if (cond) onTrue else onFalse

}
