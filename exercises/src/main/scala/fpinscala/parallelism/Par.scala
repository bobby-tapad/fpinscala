package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]
  
  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  
  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true 
    def get(timeout: Long, units: TimeUnit) = get 
    def isCancelled = false 
    def cancel(evenIfRunning: Boolean): Boolean = false 
  }
  



  case class Map2Future[A,B,C](a: Future[A], b: Future[B], f: (A,B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None
    def isDone = cache.isDefined
    def isCancelled = a.isCancelled || b.isCancelled
    def cancel(evenIfRunning: Boolean): Boolean = {
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
    }
    def get = compute(Long.MaxValue)
    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))


    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop-start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }

  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  // 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight(unit(List.empty[A]))((pa, pas) => map2(pa, pas)((a, as) => a :: as))

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  // 7.6
  def parFilter[A](l: List[A])(f: A => Boolean): Par[List[A]] = {
    val lpl: List[Par[List[A]]] = l map(asyncF((a: A) => if (f(a)) List(a) else List()))
    map(sequence(lpl))(l => l.flatten)
  }

  def parFilter2[A](l: List[A])(f: A => Boolean): Par[List[A]] = l match {
    case Nil => unit(Nil)
    case head :: tl =>
      val ph = lazyUnit {
        if (f(head)) List(head) else Nil
      }
      val pt = parFilter2(tl)(f)
      map2(ph, pt)(_ ::: _)
  }
  
  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] { 
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] = 
    map2(pa, unit(()))((a,_) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean = 
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] = 
    es => fa(es)

  // choice from the book
//  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
//    es =>
//      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
//      else f(es)

  // 7.11
  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es =>
      val n = (run(es)(pn).get)
      choices(n)(es)
  }

  // 7.11 choice in terms of choiceN
  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    es =>
      if (run(es)(cond).get)  choiceN(unit(0))(List(t))(es)
      else choiceN(unit(0))(List(f))(es)
  }

  // 7.12
  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    es =>
      choices(run(es)(key).get)(es)
  }

  // 7.13
  def flatMap[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es =>
      val a = run(es)(pa).get
      run(es)(choices(a))
  }

  // 7.13
  def flatMapChoice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = flatMap(cond)(b => if (b) t else f)


  // 7.13
  def flatMapChoiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = flatMap(n)(i => choices(i))

  // 7.14
  def join[A](a: Par[Par[A]]): Par[A] = { es => run(es)(a(es).get()) }

  // 7.14
  def flatMapWithJoin[A,B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    join(map(pa)(choices))
  }

  // 7.14
  def joinWithFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)((pa: Par[A]) => pa)


  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {

    def flatMap[B](f: A => Par[B]): Par[B] = Par.flatMap(p)(f)
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
  }
}

object Examples {
  import Par._
  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else { 
      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

  def sum2(ints: Vector[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length/2)
      Par.map2(sum2(l),sum2(r))(_ + _)
    }

}
