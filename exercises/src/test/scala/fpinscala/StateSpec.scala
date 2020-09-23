package fpinscala

import fpinscala.state.{ RNG }
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StateSpec extends AnyFlatSpec with Matchers {

  it should "sequence" in {
    val r = RNG.Simple(42)
    RNG.sequence(List(RNG.unit(1), RNG.unit(2), RNG.unit(3)))(r)._1 shouldBe List(1, 2, 3)
  }
}
