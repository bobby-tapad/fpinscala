package fpinscala.errorhandling


import scala.{Either => _, Option => _, Some => _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def flatMap2[B](f: A => Option[B]): Option[B] = map(x => f(x)).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def orElse2[B>:A](ob: => Option[B]): Option[B] = map(x => Some[B](x)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }

  def filter2(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) this else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  //math.pow(x - m, 2)
  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap(m => mean(xs map(x => math.pow(x - m, 2))))
  def variance2(xs: Seq[Double]): Option[Double] = mean(xs) flatMap2(m => mean(xs map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(a), Some(b)) => Some(f(a, b))
  }

  def map2b[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a flatMap(a1 => b map(b1 => f(a1, b1)))


  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case _ => a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((x, y) => map2(f(x), y)(_ :: _))

  //sequence in terms of traverse
  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  def sequence3[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case _ => a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2b(x, y)(_ :: _))
  }
}
