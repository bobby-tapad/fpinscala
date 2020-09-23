package fpinscala.state


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
  def nonNegativeInt(rng: RNG): (Int, RNG) = ???

  // 6.2
  // Generate a double between 0 and 1, not including 1.  Use Int.MaxValue and _.toDouble
  def double(rng: RNG): (Double, RNG) = ???

  // 6.3 use functions already written
  def intDouble(rng: RNG): ((Int,Double), RNG) = ???

  // 6.3 use functions already written
  def doubleInt(rng: RNG): ((Double,Int), RNG) = ???

  // 6.3 use functions already written
  def double3(rng: RNG): ((Double,Double,Double), RNG) = ???

  // 6.4 list of random Ints
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = ???

  // 6.5 use map to implement double

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ???

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = ???

  // 6.7 use sequence and List.fill(n)(x) to implement ints

  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???

  // 6.8 use flatMap to implement nonNegativeLessThan

  // 6.9 reimplement map and map2 in terms of flatmap
}

// 6.10 Generalize the functions unit, map, map2, flatMap, and sequence
// on the State case class where possible, otherwise on the companion object

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    ???
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    ???
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    ???
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???

  // Inserting a coin into a locked machine will cause it to unlock
  // Turning the knob on an unlocked machine will dispense candy and become locked
  // Turning the knob on a locked machine or inserting a coin in an unlocked machine does nothing
  // A machine with no candy ignores all inputs
  // need methods get, set, modify
}
