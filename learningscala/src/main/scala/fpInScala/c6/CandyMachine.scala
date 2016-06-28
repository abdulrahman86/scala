package fpInScala.c6

import fpInScala.c6.State.State

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object CandyMachine {

  def cmTransitionFunction(input: Input) : State[Machine, Int] =
    s => {
      input match {
        case Coin if s.locked && s.candies > 0 => (0, Machine(false, s.candies - 1, s.coins + 1))
        case Turn if !s.locked  => (1, Machine(true, s.candies, s.coins))
        case _ => (0, s)
      }
    }

  def simulateMachine(inputs: List[Input]): State[Machine, Int] =
    inputs.foldRight[State[Machine, Int]](State.unit((0)))((input, state) => {

      State.flatMap(cmTransitionFunction(input))(_ => state)

    })

}
