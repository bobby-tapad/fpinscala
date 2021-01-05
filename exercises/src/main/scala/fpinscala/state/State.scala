package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // 6.1
  // Use RNG.nextInt to generate a random number between 0 and Int.MaxValue inclusive.  Corner case
  // when nextInt returns Int.MinValue, which doesn't have a negative counterpart
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    if (i < 0) (-(i + 1), r) else (i, r)
  }

  def boolean(rng: RNG): (Boolean, RNG) = {
    val (i, r) = rng.nextInt
    if (i % 2 == 0) (true, r) else (false, r)
  }

  // 6.2
  // Generate a double between 0 and 1, not including 1.  Use Int.MaxValue and _.toDouble
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // 6.3 use functions already written
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  // 6.3 use functions already written
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  // 6.3 use functions already written
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // 6.4 list of random Ints
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) (Nil, rng)
    else {
      val (i, r1) = rng.nextInt
      val (l, r2) = ints(count - 1)(r1)
      (i :: l, r2)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    def helper(count: Int, acc: List[Int], r1: RNG): (List[Int], RNG) = {
      if (count <= 0) (acc, r1)
      else {
        val (i, r2) = r1.nextInt
        helper(count - 1, i :: acc, r2)
      }
    }

    helper(count, List(), rng)
  }

  // 6.5 use map to implement double
  def doubleMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, r2) = ra(rng)
    val (b, r3) = rb(r2)
    (f(a, b), r3)
  }

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(List[A]()))((ra, rb) => map2(ra, rb)(_ :: _))
  }

  // 6.7 use sequence and List.fill(n)(x) to implement ints
  def ints2(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(int))
  }

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  // 6.8 use flatMap to implement nonNegativeLessThan
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) {
    i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  // 6.9 reimplement map and map2 in terms of flatmap
  def mapfm[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => unit(f(a)))
  }

  def map2fm[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

}

// 6.10 Generalize the functions unit, map, map2, flatMap, and sequence
// on the State case class where possible, otherwise on the companion object

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s1 => {
    val (a, s2) = run(s1)
    f(a).run(s2)
  })

}


sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] = {
    fs.foldRight(unit[S, List[A]](List[A]()))((sa, sb) => sa.map2(sb)(_ :: _))
  }

  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  // Inserting a coin into a locked machine will cause it to unlock
  // Turning the knob on an unlocked machine will dispense candy and become locked
  // Turning the knob on a locked machine or inserting a coin in an unlocked machine does nothing
  // A machine with no candy ignores all inputs

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

    def resolveInputs(inputs: List[Input], machine: Machine): Machine = inputs match {
      case Nil => machine
      case input :: t => resolveInputs(t, resolveInput(input, machine))
    }

    def resolveInput(input: Input, machine:Machine): Machine = (input, machine) match {
      case (_, Machine(_, 0, _)) => machine
      case (Coin, Machine(true, candies, coins)) => Machine(false, candies, coins + 1)
      case (Turn, Machine(false, candies, coins)) => Machine(true, candies - 1, coins)
      case _ => machine
    }

    def run: Machine => ((Int, Int), Machine) = (machine: Machine) => {
      val m2 = resolveInputs(inputs, machine)
      ((m2.coins, m2.candies), m2)
    }

    State(run)
  }

}
