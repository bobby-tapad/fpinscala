package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  //5.1
  def toList(): List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, Nil).reverse
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  //5.2
  def take(n: Int): Stream[A] = this match {
    case _ if n == 0 => empty
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) => cons(h(), t().take(n - 1))
  }

  //5.2
  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n >= 1 => t().drop(n - 1)
    case _ => this
  }

  //5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  //5.4
  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b )

  //5.5 takeWhile with foldRight
  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else Empty)

  //5.6 headOption with foldRight
  def headOption: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(Empty: Stream[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Empty: Stream[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B >: A](x: => Stream[B]): Stream[B] = foldRight(x)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])((a, b) => f(a) append b)

  //5.13 map, take, takeWhile, zipWith, zipAll using unfold
  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case Empty => None
  }

  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), x) if x > 0 => Some(h(), (t(), x - 1))
    case _ => None
  }

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith[B >: A](s: Stream[B])(f: (B, B) => B): Stream[B] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), Empty))
    case (Empty, Cons(h, t)) => Some((None, Some(h())), (Empty, t()))
    case _ => None
  }

  def zipAllWith[B >: A](s: Stream[B])(f: (B, B) => B): Stream[B] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case (Cons(h, t), Empty) => Some(h(), (t(), Empty))
    case (Empty, Cons(h, t)) => Some(h(), (Empty, t()))
    case _ => None
  }

  //5.14
  def startsWith[B](s: Stream[B]): Boolean = zipAll(s).forAll(x => x match {
    case (Some(a), Some(b)) => a == b
    case (_, None) => true
    case _ => false
  })

  def startsWith2[B](s: Stream[B]): Boolean = zipAll(s).takeWhile(!_._2.isEmpty).forAll(x => x._1 == x._2)

  //5.15
  def tails: Stream[Stream[A]] = unfold (this) {
    case Cons(h, t) => Some(Cons(h, t), t())
    case Empty => None
  } append Stream(empty)

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  //5.8 infinite stream
  def constant[A](a: => A): Stream[A] = cons(a, constant(a))

  //5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  //5.10 fibonacci
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))

    go(0, 1)
  }

  //5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty
  }

  //5.12 fibs, from, constant, ones in terms of unfold
  def ones2: Stream[Int] = unfold(1)(x => Some(x, x))

  def constant2[A](a: => A): Stream[A] = unfold(a)(x => Some(x, x))

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def fibs2: Stream[Int] = Stream(0, 1) append unfold((0, 1))(x => Some(x._1 + x._2, (x._2, x._1 + x._2)))
}