package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(v) => v
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  //generalize over previous 4 functions
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
    case Leaf(v) => l(v)
    case Branch(lt, rt) => b(fold(lt)(l)(b), fold(rt)(l)(b))
  }

  //rewrite previous 4 in terms of fold
  def sizeFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => 1 + a + b)

  def maximumFold(t: Tree[Int]): Int = fold(t)(a => a)((a, b) => a max b)

  def depthFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((a, b) => (a max b) + 1)

  def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_, _))
}