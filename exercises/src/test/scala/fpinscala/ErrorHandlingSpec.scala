package fpinscala

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import fpinscala.errorhandling.{ Option, Some, None, Either, Right, Left }

class ErrorHandlingSpec extends AnyFlatSpec with Matchers {


  it should "calculate variance" in {
    val s = Seq[Double](1.0, 3.0, 4.0, 6.0)
    Option.variance(s) shouldBe Some(3.25)
    Option.variance2(s) shouldBe Some(3.25)
  }

  it should "get sequence" in {
    val l1: List[Option[Int]] = List(Some(1), Some(5), Some(3), Some(5))
    val l2: List[Option[Int]] = List(Some(1), Some(5), None, Some(3), Some(5))

    Option.sequence(l1) shouldBe Some(List(1, 5, 3, 5))
    Option.sequence(l2) shouldBe None
    Option.sequence2(l1) shouldBe Some(List(1, 5, 3, 5))
    Option.sequence2(l2) shouldBe None
    Option.sequence3(l1) shouldBe Some(List(1, 5, 3, 5))
    Option.sequence3(l2) shouldBe None
  }

  it should "traverse of either" in {
    Either.traverse(List(1, 2, 3, 4))(x => Either.Try(x + 1)) shouldBe Right(List(2, 3, 4, 5))
    Either.traverse2(List(1, 2, 3, 4))(x => Either.Try(x + 1)) shouldBe Right(List(2, 3, 4, 5))
  }

  it should "sequence of either" in {
    val l1: List[Either[String, Int]] = List(Right(1), Right(5), Right(3), Right(5))
    val l2: List[Either[String, Int]] = List(Right(1), Right(5), Left("oops"), Right(3), Right(5))

    Either.sequence(l1) shouldBe Right(List(1, 5, 3, 5))
    Either.sequence(l2) shouldBe Left("oops")
  }
}
