package fpinscala

import fpinscala.laziness.{ Stream, Empty, Cons}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LazinessSpec extends AnyFlatSpec with Matchers {


  it should "convert a stream to a list" in {
    Stream(1, 2, 3, 4).toList() shouldBe List(1, 2, 3, 4)
    Stream().toList() shouldBe Nil
  }

  it should "take" in {
    Stream(1, 2, 3, 4).take(2).toList() shouldBe List(1, 2)
    Stream(1, 2, 3, 4).take(1).toList() shouldBe List(1)
    Stream(1, 2, 3, 4).take(0).toList() shouldBe Nil
    Stream(1, 2, 3, 4).take(5).toList() shouldBe List(1, 2, 3, 4)
  }

  it should "drop" in {
    Stream(1, 2, 3, 4).drop(2).toList() shouldBe List(3, 4)
    Stream(1, 2, 3, 4).drop(1).toList() shouldBe List(2, 3, 4)
    Stream(1, 2, 3, 4).drop(0).toList() shouldBe List(1, 2, 3, 4)
    Stream(1, 2, 3, 4).drop(10).toList() shouldBe Nil
  }

  it should "takewhile" in {
    Stream(1, 2, 3, 4).takeWhile(_ == 1).toList() shouldBe List(1)
    Stream(1, 2, 3, 4).takeWhile(_ == 2).toList() shouldBe Nil
    Stream(1, 1, 1, 2, 3, 4).takeWhile(_ == 1).toList() shouldBe List(1, 1, 1)
  }

  it should "forall" in {
    Stream(1, 2, 3, 4).forAll(_ > 0) shouldBe true
    Stream.cons(1, Stream.cons(2, Stream.cons(-3, {throw new Exception(" :( ")}))).forAll(_ > 0) shouldBe false
    Stream(1, 2, -3, 4).forAll(_ > 0) shouldBe false

    def nat(start: Int): Stream[Int] = Stream.cons(start, nat(start + 1))
    nat(1).forAll(_ % 10 != 0) shouldBe false
  }

  it should "takewhile with foldright" in {
    Stream(1, 2, 3, 4).takeWhile2(_ == 1).toList() shouldBe List(1)
    Stream(1, 2, 3, 4).takeWhile2(_ == 2).toList() shouldBe Nil
    Stream(1, 1, 1, 2, 3, 4).takeWhile2(_ == 1).toList() shouldBe List(1, 1, 1)
  }

  it should "headoption" in {
    Stream(1, 2, 3, 4).headOption shouldBe Some(1)
    Stream().headOption shouldBe None
    Stream.empty.headOption shouldBe None
  }

  it should "map" in {
    Stream(1, 2, 3, 4).map(_ + 1).toList() shouldBe List(2, 3, 4, 5)
    Stream[Int]().map(_ + 1) shouldBe Empty
  }

  it should "filter" in {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList() shouldBe List(2, 4)
    Stream(1, 3).filter(_ % 2 == 0) shouldBe Empty
    Stream[Int]().filter(_ % 2 == 0) shouldBe Empty
  }

  it should "append" in {
    Stream(1, 2, 3, 4).append(Stream(5)).toList() shouldBe List(1, 2, 3, 4, 5)
    Stream.empty.append(Stream(1)).toList() shouldBe List(1)
  }

  it should "flatMap" in {
    Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList() shouldBe List(1, 1, 2, 2, 3, 3)
    Stream().flatMap(i => Stream(i, i)) shouldBe Empty
  }

  it should "make infinite stream" in {
    Stream.constant(1).take(5).toList() shouldBe List(1, 1, 1, 1, 1)
  }

  it should "add 1 infinitely" in {
    Stream.from(1).take(5).toList() shouldBe List(1, 2, 3, 4, 5)
  }

  it should "fibonacci" in {
    Stream.fibs.take(7).toList() shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  it should "unfold" in {
    Stream.unfold(1)(n => if (n <= 5) Some((n, n + 1)) else None).toList() shouldBe List(1, 2, 3, 4, 5)
  }

  it should "unfold ones" in {
    Stream.ones2.take(5).toList() shouldBe List(1, 1, 1, 1, 1)
  }

  it should "unfold constant" in {
    Stream.constant2(1).take(5).toList() shouldBe List(1, 1, 1, 1, 1)
  }

  it should "unfold from" in {
    Stream.from2(1).take(5).toList() shouldBe List(1, 2, 3, 4, 5)
  }

  it should "unfold fibonacci" in {
    Stream.fibs2.take(7).toList() shouldBe List(0, 1, 1, 2, 3, 5, 8)
  }

  it should "unfold map" in {
    Stream(1, 2, 3, 4).map2(_ + 1).toList() shouldBe List(2, 3, 4, 5)
    Stream[Int]().map2(_ + 1) shouldBe Empty
  }

  it should "unfold take" in {
    Stream(1, 2, 3, 4).take2(2).toList() shouldBe List(1, 2)
    Stream(1, 2, 3, 4).take2(1).toList() shouldBe List(1)
    Stream(1, 2, 3, 4).take2(0).toList() shouldBe Nil
    Stream(1, 2, 3, 4).take(5).toList() shouldBe List(1, 2, 3, 4)
  }

  it should "unfold takewhile" in {
    Stream(1, 2, 3, 4).takeWhile3(_ == 1).toList() shouldBe List(1)
    Stream(1, 2, 3, 4).takeWhile3(_ == 2).toList() shouldBe Nil
    Stream(1, 1, 1, 2, 3, 4).takeWhile3(_ == 1).toList() shouldBe List(1, 1, 1)
  }

  it should "zipwith" in {
    Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ + _).toList() shouldBe List(5, 7, 9)
  }

  it should "zipallwith" in {
    Stream(1, 2, 3).zipAllWith(Stream(4, 5, 6, 7, 8, 9))(_ + _).toList() shouldBe List(5, 7, 9, 7, 8, 9)
    Stream(1, 2, 3, 4, 5, 6).zipAllWith(Stream(4, 5, 6))(_ + _).toList() shouldBe List(5, 7, 9, 4, 5, 6)
  }

  it should "zipall" in {
    val s1 = Stream(1, 2)
    val s2 = Stream(3)

    s1.zipAll(s2).toList() shouldBe List((Some(1), Some(3)), (Some(2), None))
    s2.zipAll(s1).toList() shouldBe List((Some(3), Some(1)), (None, Some(2)))
    s1.zipAll(Empty).toList() shouldBe List((Some(1), None), (Some(2), None))
  }

  it should "startsWith" in {
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2).startsWith(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3).startsWith(Stream(1, 3)) shouldBe false

    Stream(1, 2, 3).startsWith2(Stream(1, 2)) shouldBe true
    Stream(1, 2).startsWith2(Stream(1, 2)) shouldBe true
    Stream(1, 2, 3).startsWith2(Stream(1, 3)) shouldBe false
  }

  it should "tails" in {
    Stream(1, 2, 3).tails.map(_.toList()).toList() shouldBe List(List(1, 2, 3), List(2, 3), List(3), List())
  }

}
