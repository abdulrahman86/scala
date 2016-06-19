package fpInScala.c6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xffffffffffffl
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A) : Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rRng) = s(rng)
      (f(a), rng)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rRng1) = ra(rng)
      val (b, rRng2) = rb(rRng1)
      (f(a, b), rRng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))


  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double2: Rand[Double] =
    map(int)(_.toDouble / Integer.MAX_VALUE)

  @tailrec
  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (x, y) if x != 0 => (Math.abs(x),y)
    case (_, y) => nonNegativeInt(y)
  }

  def double(rng: RNG): (Double, RNG) = nonNegativeInt(rng) match {
    case(x, y) => ((x.toDouble / Integer.MAX_VALUE), y)
  }

  def double3(rng: RNG) : ((Double, Double, Double), RNG) = {

    val (r1, nRNG1) = double(rng)
    val (r2, nRNG2) = double(nRNG1)
    val (r3, nRNG3) = double(nRNG2)

    ((r1, r2, r3), nRNG3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if(count == 0) (List(), rng)
    else {
      val (r1, rng1) = rng.nextInt
      val(tail, rng2) = ints(count - 1)(rng1)
      (r1 :: tail, rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight[Rand[List[A]]](unit(List())){(x: Rand[A], y: Rand[List[A]]) =>{
      (rng: RNG) => {
        val (tail, rng1) = y(rng)
        val (head, rng2) = x(rng1)
        (head :: tail, rng2)
      }
    }}

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(count)(int))(rng)

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rngNext) = f(rng)
      g(a)(rngNext)
    }

  def map3[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap[A, B](s)(x => unit(f(x)))

  def map4[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    map3[(A, B), C](flatMap[A, (A,B)](ra)(a => {
      rng => {
        val (b, r) = rb(rng)
        ((a, b),r)
      }
    }))(x => f(x._1, x._2))

  def both2[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map4(ra, rb)((_, _))


}
