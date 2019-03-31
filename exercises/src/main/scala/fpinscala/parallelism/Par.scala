package fpinscala.parallelism

import java.util.concurrent._

import scala.language.implicitConversions
import scala.util.Try

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

  // 7.1 - unfortunate spoiler
  def map2Original[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values, waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available time allocated for evaluating `bf`.
    }


  case class MappedFuture[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C, es: ExecutorService) extends Future[C] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
      val ca = fa.cancel(mayInterruptIfRunning)
      val cb = fb.cancel(mayInterruptIfRunning)
      ca || cb
    }

    override def isCancelled: Boolean = fa.isCancelled || fb.isCancelled

    // v0.1 did not have `result` cache and `isDone` was defined as `fa.isDone && fb.isDone`,
    // which is not exactly the same as "result is fully calculated with `f(a,b)`"
    override def isDone: Boolean = result.isDefined

    override def get(): C = merge(Long.MaxValue)

    override def get(timeout: Long, unit: TimeUnit): C = {
      val totalNanos = TimeUnit.NANOSECONDS.convert(timeout, unit)
      merge(totalNanos)
    }

    private def merge(totalNanos: Long): C = result match {
      case Some(c) => c
      case None =>
        val t0 = System.nanoTime()
        val a = fa.get(totalNanos, TimeUnit.NANOSECONDS)
        val t1 = System.nanoTime()
        val remainingNanos = totalNanos - (t1 - t0)
        val b = fb.get(remainingNanos, TimeUnit.NANOSECONDS)
        // don't call `merge` from multiple Threads!
        val c = f(a, b)
        result = Some(c)
        c
    }

    @volatile private var result: Option[C] = None
  }

  // 7.3
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      MappedFuture(af, bf, f, es)
    }

  //  // Brain fart: `map` is not supposed to know about timeouts, `get` is
  //  case class Patience(value: Duration)
  //
  //  def map2WithPatience[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C)(implicit patience: Patience): Par[C] = (es: ExecutorService) => {
  //    val t0 = System.nanoTime()
  //    val totalNanos = patience.value.toNanos
  //    val af = a(es)
  //    val bf = b(es)
  //    val resultA = af.get(totalNanos, TimeUnit.NANOSECONDS)
  //    val t1 = System.nanoTime()
  //    val remainingNanos = totalNanos - (t1 - t0)
  //    // Note: Future.get takes care of timeout remainingNanos < 0
  //    val resultB = bf.get(remainingNanos, TimeUnit.NANOSECONDS)
  //    UnitFuture(f(resultA, resultB))
  //  }

  def fork[A](a: => Par[A]): Par[A] =
  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--
  // for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism.
  // Essentially, we're using two threads when one should suffice. This is a symptom of a more serious problem with the
  // implementation, and we will discuss this later in the chapter.
    es => es.submit(() => a(es).get)


  // 7.4
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit(List.empty)) { (pa, pas) =>
    map2(pa, pas) { (a, as) => a :: as }
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  // 7.6
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    //map(unit(as))(_.filter(f))
    def pass(a: A) = if (f(a)) Option(a) else None

    val fbs: List[Par[Option[A]]] = as.map(asyncF(pass))
    map(sequence(fbs))(_.flatten)
  }

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)

    def map2[B, C](b: Par[B])(f: (A, B) => C): Par[C] = Par.map2(p, b)(f)

    def zip[B](b: Par[B]): Par[(A, B)] = p.map2(b)((_, _))
  }

  /* 7.7
  * given map(y)(id) == y
  * prove map(map(y)(g))(f) == map(y)(f ∙ g)
  * prove map(_)(f) ∙ map(_)(g) ~= map(_)(f ∙ g)
  * let m(x) = map(_)(x)
  * given m(id) ~= id
  * prove m(f) ∙ m(g) ~= m(f ∙ g)
  * see https://github.com/quchen/articles/blob/master/second_functor_law.md
  * */


}

object Examples {

  import Par._

  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
    if (ints.size <= 1)
      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
    else {
      val (l, r) = ints.splitAt(ints.length / 2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }


  def sumPar(ints: IndexedSeq[Int]): Par[Int] = if (ints.length <= 1)
    Par.unit(ints.headOption getOrElse 0) else {
    val (l, r) = ints.splitAt(ints.length / 2)
    Par.map2(Par.fork(sumPar(l)), Par.fork(sumPar(r)))(_ + _)
  }

}

object TestPar extends App {

  import Par._

  import java.util.concurrent._

  val es = Executors.newFixedThreadPool(8)

  import TimeUnit.SECONDS

  //  def a(es: ExecutorService) = es.submit(new Callable[Int] {
  //    override def call(): Int = {
  //      import scala.concurrent.duration._
  //      Thread.sleep(1.second.toMillis)
  //      10
  //    }
  //  })
  //  println(Try(run(es)(map2(a, a)(_ + _)).get(5, SECONDS)))
  //  println(Try(run(es)(map2(a, a)(_ + _)).get(2, SECONDS)))
  //  println(Try(run(es)(map2(a, a)(_ + _)).get(1500, MILLISECONDS)))
  //  println(Try(run(es)(map2(a, a)(_ + _)).get(1, SECONDS)))
  //  println(Try(run(es)(map2(a, a)(_ + _)).get(0, SECONDS)))


  // def printPar[A](p: Par[A]) = println(Try(run(es)(p).get(10, SECONDS)))
  // printPar(parMap(List(1, 2, 3, 4, 5, 6, 7, 8))(_ * 2))

  def longOne() = {
    (1 to 10).foreach { i =>
      Thread.sleep(1000)
      println(s"Still ticking: $i")
    }
    1
  }

  //  println(Try(run(es)(fork(unit(longOne))).get(1, SECONDS)))
  //  println(Try(run(es)(unit(longOne)).get(1, SECONDS)))
  val u = lazyUnit(longOne)
  println(u)
  val r = run(es)(u)
  println(r)
  println(Try(r.get(1, SECONDS)))


  Thread.sleep(2000)
  es.shutdown()
  sys.exit()

}


/* 7.8
* fork(x) ~= x
* Due to the limitations described in the original code of fork, a multiply forked Par cannot run on a single-threaded executor
* */
object TestSingleTPar extends App {

  import Par._

  import java.util.concurrent._

  val es = Executors.newSingleThreadScheduledExecutor()

  import TimeUnit.SECONDS

  def printPar[A](p: Par[A]) = println(Try(run(es)(p).get(1, SECONDS)))

  printPar(unit(1)) // Success(1)
  printPar(fork(unit(1))) // Success(1)
  printPar(fork(fork(unit(1)))) // Failure(java.util.concurrent.TimeoutException

  es.shutdown()
  sys.exit()
}

/* 7.9
* Given a thread pool of fixed size there could always be constructed a program with n+1 fork nesting
* */

object TestDeadlockPar extends App {

  import Par._

  import java.util.concurrent._
  import TimeUnit.SECONDS

  def printPar[A](p: Par[A]) = println(Try(run(es)(p).get(1, SECONDS)))

  val n = 16 // any n
  val es = Executors.newFixedThreadPool(n)
  val p = fork(sequence((1 to n).toList.map(i => fork(unit(i)))).map(_.sum))
  printPar(p) // Success(136)
  val d = (1 to n + 1).foldLeft[Par[Unit]](unit(()))((par, _) => fork(par))
  printPar(d) // Failure(java.util.concurrent.TimeoutException)

  es.shutdown()
  sys.exit()
}