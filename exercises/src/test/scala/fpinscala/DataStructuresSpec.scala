package fpinscala

import fpinscala.datastructures._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class DataStructuresSpec extends AnyFlatSpec with Matchers {

  //lists

  it should "return the tail of a list" in {
    val l = List(1, 2, 3, 4, 5)
    List.tail(l) shouldBe List(2, 3, 4, 5)
  }

  it should "set the head of a list" in {
    val l = List(1, 2, 3, 4, 5)
    val n = Nil

    List.setHead(l, 99) shouldBe List(99, 2, 3, 4, 5)
    List.setHead(n, 99) shouldBe List(99)
  }

  it should "drop elements of a list" in {
    val l = List(1, 2, 3, 4, 5)

    List.drop(l, 0) shouldBe List(1, 2, 3, 4, 5)
    List.drop(l, 1) shouldBe List(2, 3, 4, 5)
    List.drop(l, 3) shouldBe List(4, 5)
    List.drop(l, 10) shouldBe Nil
  }

  it should "dropWhile" in {
    val l = List(1, 1, 1, 2, 3, 4, 5)

    List.dropWhile(l, (a: Int) => a == 0) shouldBe List(1, 1, 1, 2, 3, 4, 5)
    List.dropWhile(l, (a: Int) => a == 1) shouldBe List(2, 3, 4, 5)
    List.dropWhile(Nil, (a: Int) => a == 0) shouldBe Nil
  }

  it should "init" in {
    val l = List(1, 2, 3, 4, 5)

    List.init(l) shouldBe List(1, 2, 3, 4)
    List.init(Nil) shouldBe Nil
    List.init(List(1)) shouldBe Nil
  }

  it should "length" in {
    List.length(List(1, 2, 3, 4)) shouldBe 4
    List.length(Nil) shouldBe 0
    List.length(List(1)) shouldBe 1
  }

  it should "length with foldRight" in {
    List.length2(List(1, 2, 3, 4)) shouldBe 4
    List.length2(Nil) shouldBe 0
    List.length2(List(1)) shouldBe 1
  }

  it should "foldLeft" in {
    List.sumFL(List(1, 2, 3, 4)) shouldBe List.sum2(List(1, 2, 3, 4))
    List.productFL(List(1, 2, 3, 4)) shouldBe List.product2(List(1, 2, 3, 4))
  }

  it should "reverse" in {
    List.reverse(List(1, 2, 3, 4)) shouldBe List(4, 3, 2, 1)
  }

  it should "foldRightTailRec" in {
    List.sumFL(List(1, 2, 3, 4)) shouldBe List.sum2(List(1, 2, 3, 4))
    List.productFL(List(1, 2, 3, 4)) shouldBe List.product2(List(1, 2, 3, 4))
  }

  it should "appendFold" in {
    List.appendFold(List(1, 2, 3), List(8, 9, 10)) shouldBe List(1, 2, 3, 8, 9, 10)
  }

  it should "concatenate" in {
    List.concatenate(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))) shouldBe List.apply(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  it should "addOne" in {
    List.addOne(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  it should "doublesToStrings" in {
    List.doublesToStrings(List(1, 2, 3)) shouldBe List("1.0", "2.0", "3.0")
  }

  it should "map" in {
    List.map(List(1, 2, 3))(a => a + 1) shouldBe List(2, 3, 4)
  }

  it should "filter odd numbers" in {
    List.filter(List(1, 2, 3, 4, 5))(a => a % 2 == 0) shouldBe List(2, 4)
  }

  it should "flatMap" in {
    List.flatMap(List(1, 2, 3))(i => List(i, i)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  it should "filterFlatMap" in {
    List.filterFlatMap(List(1, 2, 3, 4, 5))(a => a % 2 == 0) shouldBe List(2, 4)
  }

  it should "addLists" in {
    List.addLists(List(1, 2, 3), List(4, 5, 6)) shouldBe List(5, 7, 9)
  }

  it should "zipWith" in {
    List.zipWith(List(1, 2, 3), List(4, 5, 6))((a: Int, b: Int) => a + b) shouldBe List(5, 7, 9)
  }

  it should "hasSubsequence" in {
    List.hasSubsequence(List(1, 2, 3, 5, 6, 9), List(3, 5)) shouldBe true
    List.hasSubsequence(List(1, 2, 3), List(2, 2)) shouldBe false
  }

  //trees
  val tree: Tree[Int] = Branch(Branch(Leaf(4), Branch(Leaf(2), Leaf(1))), Leaf(3))

  it should "get tree size" in {
    Tree.size(tree) shouldBe 7
  }

  it should "get tree max" in {
    Tree.maximum(tree) shouldBe 4
  }

  it should "get tree depth" in {
    Tree.depth(tree) shouldBe 4
  }

  it should "tree map" in {
    Tree.map(tree)(a => a + 1) shouldBe Branch(Branch(Leaf(5), Branch(Leaf(3), Leaf(2))), Leaf(4))
  }

  it should "get tree size with fold" in {
    Tree.sizeFold(tree) shouldBe 7
  }

  it should "get tree max with fold" in {
    Tree.maximumFold(tree) shouldBe 4
  }

  it should "get tree depth with fold" in {
    Tree.depthFold(tree) shouldBe 4
  }

  it should "tree map with fold" in {
    Tree.mapFold(tree)(a => a + 1) shouldBe Branch(Branch(Leaf(5), Branch(Leaf(3), Leaf(2))), Leaf(4))
  }
}