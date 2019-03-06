package PurelyFunctionalState

/*
 * the use of State, implemented a finite state automaton
 * that models a simple candy dispenser. The machine has two types of input: you can
 * insert a coin, or you can turn the knob to dispense candy. It can be in one of two
 * states: locked or unlocked. It also tracks how many candies are left and how many
 * coins it contains.
 * The rules of the machine are as follows:
 * Inserting a coin into a locked machine will cause it to unlock if there’s any
 * candy left.
 * Turning the knob on an unlocked machine will cause it to dispense candy and
 * become locked.
 * Turning the knob on a locked machine or inserting a coin into an unlocked
 * machine does nothing.
 * A machine that’s out of candy ignores all inputs.
 */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object dispenser extends App {

  val unit: Machine => ((Int, Int), Machine) = (m: Machine) => ((m.candies, m.coins), m)

  val state_update: Input => State[Machine, (Int, Int)] = (i: Input) =>
    State({ m: Machine =>
      i match {
        case Coin => if (m.locked && m.candies > 0) unit(Machine(false, m.candies, m.coins + 1)) else unit(m)
        case Turn => if (!m.locked && m.candies > 0) unit(Machine(true, m.candies - 1, m.coins)) else unit(m)
      }
    })

  val stimulateMachine =
    (inputs: List[Input]) => State.sequence(inputs.map(i => state_update(i)))

  val finalState =
    (inputs: List[Input]) => State[Machine, (Int, Int)](m => unit(stimulateMachine(inputs).run(m)._2))

  val m = Machine(true, 5, 10)
  val inputs = List(Coin, Turn, Coin, Turn, Coin, Coin, Turn, Turn)
  println(stimulateMachine(inputs).run(m))
  println(finalState(inputs).run(m))
}


