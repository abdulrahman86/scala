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
}
