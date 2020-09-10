package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new Exception
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

  def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = l match {
    case Nil => 0
    case Cons(_, Nil) => 1
    case Cons(_, t) => 1 + length(t)
  }

  //length with foldRight
  def length2[A](l: List[A]): Int = foldRight(l, 0)((_: A, b: Int) => b + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  def sumFL(ns: List[Int]) =
    foldLeft(ns, 0)((x,y) => x + y)

  def productFL(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((y, x) => Cons(y, x))

  def foldRightTailRec[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)(f)

  //append using fold
  def appendFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x, y) => Cons[A](x, y))

  //concat a list of lists
  def concatenate[A](ls: List[List[A]]): List[A] = foldLeft(ls, List[A]())((x, y) => appendFold(y, x))

  //add 1 to a list of ints
  def addOne(l: List[Int]): List[Int] = foldRightTailRec(l, List[Int]())((x, y) => Cons(x + 1, y))

  def doublesToStrings(l: List[Double]): List[String] = foldRightTailRec(l, List[String]())((x, y) => Cons(x.toString, y))

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightTailRec(l, List[B]())((a, b) => Cons(f(a), b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRightTailRec(l, List[A]()) {
    (a, b) => if (f(a)) Cons(a, b) else b
  }

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRightTailRec(l, List[B]())((a, b) => append(f(a), b))

  def filterFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(a => if (f(a)) List(a) else Nil)

  //add the elements of 2 lists
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }

  //generalize addLists
  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = {
    def startsWith(sub: List[A], l: List[A]): Boolean = sub match {
      case Nil => true
      case Cons(sh, st) => l match {
        case Nil => false
        case Cons(lh, lt) => if (sh == lh) startsWith(lt, st) else false
      }
    }

    sub match {
      case Nil => true
      case Cons(_, _) => l match {
        case Nil => false
        case Cons(_, lt) => if (startsWith(sub, l)) true else hasSubsequence(lt, sub)
      }
    }
  }
}
