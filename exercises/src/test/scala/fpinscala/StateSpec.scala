package fpinscala

import fpinscala.state.State.simulateMachine
import fpinscala.state.{Coin, Machine, RNG, State, Turn}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateSpec extends AnyFlatSpec with Matchers {

  it should "sequence" in {
    val r = RNG.Simple(42)
    RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(r)._1 shouldBe List(1, 2, 3)
  }

  it should "Inserting a coin into a locked machine will cause it to unlock" in {
    val m1: Machine = Machine(locked = true, candies = 1, coins = 1)
    val inputs = List(Coin)

    State.simulateMachine(inputs).run(m1) shouldBe ((2, 1), Machine(false, 1, 2))
  }

  it should "Turning the knob on an unlocked machine will dispense candy and become locked" in {
    val m1: Machine = Machine(locked = false, candies = 1, coins = 1)
    val inputs = List(Turn)

    State.simulateMachine(inputs).run(m1) shouldBe ((1, 0), Machine(true, 0, 1))
  }

  it should "Turning the knob on a locked machine does nothing" in {
    val m1: Machine = Machine(locked = true, candies = 1, coins = 1)
    val inputs = List(Turn)

    State.simulateMachine(inputs).run(m1) shouldBe ((1, 1), Machine(true, 1, 1))
  }

  it should "Inserting a coin in an unlocked machine does nothing" in {
    //I am assuming "does nothing" means the machine does not accept the coin
    val m1: Machine = Machine(locked = false, candies = 1, coins = 1)
    val inputs = List(Coin)

    State.simulateMachine(inputs).run(m1) shouldBe ((1, 1), Machine(false, 1, 1))
  }

  it should "A machine with no candy ignores all inputs" in {
    val m1: Machine = Machine(locked = true, candies = 0, coins = 1)
    val inputs = List(Turn, Coin, Turn, Turn, Coin)

    State.simulateMachine(inputs).run(m1) shouldBe ((1, 0), Machine(true,0, 1))
  }

  it should "do the example from the book" in {
    val m1: Machine = Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Turn, Coin, Turn, Coin, Turn)

    State.simulateMachine(inputs).run(m1) shouldBe ((14, 1), Machine(true, 1, 14))
  }
}
