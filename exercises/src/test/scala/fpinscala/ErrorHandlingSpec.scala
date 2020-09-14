package fpinscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fpinscala.errorhandling.{ Option, Some, None }

class ErrorHandlingSpec extends AnyFlatSpec with Matchers {


  it should "calculate variance" in {
    val s = Seq[Double](1.0, 3.0, 4.0, 6.0)
    Option.variance(s) shouldBe Some(3.25)
  }

  it should "get sequence" in {
    val l1: List[Option[Int]] = List(Some(1), Some(5), Some(3), Some(5))
    val l2: List[Option[Int]] = List(Some(1), Some(5), None, Some(3), Some(5))

    Option.sequence(l1) shouldBe Some(List(1, 5, 3, 5))
    Option.sequence(l2) shouldBe None
    Option.sequence2(l1) shouldBe Some(List(1, 5, 3, 5))
    Option.sequence2(l2) shouldBe None
  }
}
