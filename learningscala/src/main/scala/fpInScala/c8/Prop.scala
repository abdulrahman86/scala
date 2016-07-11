package fpInScala.c8

import fpInScala.c6.State.State
import fpInScala.c5.Stream
import fpInScala.c6.{RNG, SimpleRNG, State}
import fpInScala.c8.Prop.{Result, TestCases}
import week4.False;

object Prop {

  case class Prop(run: (MaxSize, TestCases, RNG) => Result)

  def && (p1 : Prop)(p2: Prop): Prop = Prop {
    (maxSize, testCases: TestCases, rng: RNG) =>
      p1.run(maxSize, testCases, rng) match {
        case Falsified(x, y) => Falsified(x, y)
        case Passed => p2.run(maxSize, testCases, rng) match {
          case Falsified(x, y) => Falsified(x, testCases + y)
          case _ => Passed
        }
      }
  }

  def || (p1 : Prop)(p2: Prop): Prop = Prop(
    run = (maxSize: MaxSize, testCases: TestCases, rng: RNG) =>
      p1.run(maxSize, testCases, rng) match {
        case Passed => Passed
        case Falsified(x, y) => p2.run(maxSize, testCases, rng) match {
          case Passed => Passed
          case Falsified(x, y) => Falsified(x, 2 * testCases)
        }
      })

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result  {
    def isFalsified = true
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (maxSize, n, rng) => {
      Stream.find[Result](_.isFalsified)(Stream.map[(Option[A], Option[Int]), Result]{
        case (Some(a), Some(i)) => try {
          if(f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }(Stream.taken2[(Option[A], Option[Int])](n)(Stream.zipAll[A, Int](randomStream[A](as)(rng), Stream.from(0))))).getOrElse(Passed)
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll[A](g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val props : Stream[Prop] = Stream.map[Int, Prop](i => forAll(g(i))(f))(Stream.taken2(n)(Stream.from(0)))
      val prop: Prop = Stream.toList(Stream.map[Prop, Prop](p => Prop {
        (max, _, rng) => p.run(max, casesPerSize, rng)
      })(props)).reduce(&&(_)(_))
      prop.run(max, n, rng)
    }
  }

  def run (p: Prop, maxSize: MaxSize = 100, testCases: TestCases = 100, rng:RNG =  SimpleRNG(System.currentTimeMillis())) =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"Falsified afer $n passed tests:\n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
    }



  def buildMsg[A](s: A, e: Exception): String =
    s"test case : $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample(rng)))

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String
  type MaxSize = Int
  //type Result = Option[(FailedCase, SuccessCount)]
}

case class Gen[+A] (sample: State[RNG, A])

case class SGen[+A](forSize: Int => Gen[A])

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

  def unsized[A](in: Gen[A]): SGen[A] =
    SGen[A](_ => in)
}

object SGen {
  def map[A, B](f: A => B)(in: SGen[A]) : SGen[B] =
    SGen(size => Gen.map(f)(in.forSize(size)))

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => Gen.listOfN[A](size, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(size => Gen.listOfN[A](Math.max(1, size), g))
}