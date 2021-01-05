package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import fpinscala.state.RNG.nonNegativeInt

import java.util.concurrent.{ExecutorService, Executors}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
  def check: Either[(FailedCase,SuccessCount),SuccessCount]

  // 8.3
  //def &&(p: Prop): Prop = new Prop { this.check && p.check }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???


}

object Gen {
  // 8.5
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  // 8.5
  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))
  // 8.5
  // the answer key uses State.sequence.  is that better?
  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = g.map((a: A) => List.fill(n)(a))

}

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = ???

  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  def choosePair(start: Int, stopExclusive: Int): Gen[(Int,Int)] = Gen(choose(start, stopExclusive).sample.map(i => (i,i)))

  def genToOption(gen: Gen[A]): Gen[Option[A]] = Gen(gen.sample.map(a => Option(a)))


}

trait SGen[+A] {

}

