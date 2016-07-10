package fpInScala.c8

import fpInScala.c6.State.State
import fpInScala.c5.Stream
import fpInScala.c6.{RNG, State}
import week4.False;

trait Prop {

  def check: Boolean
  def && (p: Prop) = new Prop {
    override def check: Boolean = Prop.this.check && p.check
  }
}

case class Gen[A] (sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen[Int](rng => {

      val (result, rngNext) = rng.nextInt
      ((result % (stopExclusive - start)) + start,rngNext)
    })
  }

  def unit[A](a: => A) : Gen[A] = Gen[A](rng => {
    (a, rng)
  })

  def map[A, B](f: A => B)(in: Gen[A]): Gen[B] = Gen[B](State.map(f)(in.sample))

  def boolean: Gen[Boolean] = Gen.map[Int, Boolean](x => x match {
    case 0 => false
    case 1 => true
  })(Gen.choose(0, 2))


  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen[List[A]](rng => {
    Stream.toList(Stream.taken2(n)(Stream.unfold(rng)(s => Some{
      val (a, nextS) = g.sample(s)
      ((a, nextS), nextS)
    }))).foldLeft[(List[A], RNG)]((List(), rng))((result, elem) => (elem._1 :: result._1, result._2))
  })

  def flatMap[A, B](f: A => Gen[B])(in: Gen[A]): Gen[B] = Gen(State.flatMap(in.sample)(a => {
    f(a).sample
  }))

  def listOfN2[A](size: Gen[Int])(in: Gen[A]) : Gen[List[A]] = flatMap[Int, List[A]](result => listOfN(result, in))(size)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = Gen(rng => {
    val (result, rngNext) = Gen.boolean.sample(rng)
    if (result == 0) (g1.sample(rngNext))
    else g2.sample(rngNext)
  })

  def weighted[A](g1:(Gen[A], Double), g2: (Gen[A], Double)) : Gen[A] =
    Gen.flatMap[Int, A](x => {
      if (g1._2 /(g1._2 + g2._2) * 100 < x) g1._1
      else g2._1
    })(Gen.choose(1, 101))
}