package fpinscala

import fpinscala.datastructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class DataStructuresSpec extends AnyFlatSpec with Matchers {

  it should "return the fibonacci sequence" in {
    val hw = List.apply("Hello World!")
    List.tail(hw) shouldBe "ello World!"
  }

}