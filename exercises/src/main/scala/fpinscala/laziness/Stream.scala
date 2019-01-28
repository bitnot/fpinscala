package fpinscala.laziness

import Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // 5.1
  def toList: List[A] = {
    @tailrec
    def go(that: Stream[A], acc: List[A]): List[A] = that match {
      case Empty => acc
      case Cons(h, t) => go(t(), h() :: acc)
    }

    go(this, Nil).reverse
  }

  // 5.2
  def take(n: Int): Stream[A] = {
    def go(source: Stream[A], n: Int): Stream[A] = (n, source) match {
      case (k, _) if k <= 0 => empty
      case (_, Empty) => empty
      case (k, Cons(h, t)) => Cons(h, () => go(t(), k - 1))
    }

    go(this, n)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) =>
      val head = h() // `h` evaluates only once
      if (p(head)) Cons(() => head, () => t().takeWhile(p))
      else Empty
    case _ => this
  }

  // 5.4
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Empty => true
  }

  def forAllFR(p: A => Boolean): Boolean = this.foldRight(true)((a, b) => p(a) && b)

  // 5.5
  def takeWhileFR(p: A => Boolean): Stream[A] = this.foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // 5.6
  def headOption: Option[A] = this.foldRight(None: Option[A])((a, _) => Option(a))


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a).append(b))



  // 5.13
  def mapU[B](f: A => B): Stream[B] = ???

  def takeU(n: Int): Stream[A] = ???

  def takeWhileU(p: A => Boolean): Stream[A] = ???

  def zipWithU[B](other: Stream[B]): Stream[(A, B)] = ???

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] = ???

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean = ???

  // 5.15
  def tails: Stream[Stream[A]] = ???

  // 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = ???
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

  // 5.8
  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  // 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // 5.10
  lazy val fibs: Stream[Int] = {
    def go(f1: Int, f2: Int): Stream[Int] = cons(f1, go(f2, f1 + f2))

    go(0, 1)
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

  // 5.12
  lazy val fibsU: Stream[Int] = unfold[Int, (Int, Int)]((0, 1)) { case (f1, f2) => Some((f1, (f2, f1 + f2))) }
  lazy val onesU: Stream[Int] = unfold[Int, Int](1)(x => Some((x, x)))

  final def constantU(c: Int): Stream[Int] = unfold[Int, Int](c)(x => Some((x, x)))

  final def fromU(n: Int): Stream[Int] = unfold[Int, Int](n)(x => Some((x, x + 1)))

}


object StreamTest extends App {

  import Stream.cons

  def five() = {
    println("five");
    5
  }

  val ss: Stream[Int] = cons(1, cons(2, cons(3, cons(4, Cons(five, () => Stream.empty[Int])))))
  println("------------------------------------------------------------")
  println(s"Stream(1,2,3,4,5).toList = ${ss.toList}")
  println("------------------------------------------------------------")
  println(s"Stream(1,2,3,4,5).take(3).toList = ${ss.take(3).toList}")
  println(s"Stream(1,2,3,4,5).take(5).toList = ${ss.take(5).toList}")
  println(s"Stream(1,2,3,4,5).take(5) = ${ss.take(5)}")
  println("------------------------------------------------------------")
  println(s"Stream(1,2,3,4,5).drop(3).toList = ${ss.drop(3).toList}")
  println(s"Stream(1,2,3,4,5).drop(4).toList = ${ss.drop(4).toList}")
  println(s"Stream(1,2,3,4,5).drop(5).toList = ${ss.drop(5).toList}")
  println("------------------------------------------------------------")
  println(s"Stream(1,2,3,4,5).takeWhile(_ < 4).toList = ${ss.takeWhile(_ < 4).toList}")
  println(s"Stream(1,2,3,4,5).takeWhile(_ < 5).toList = ${ss.takeWhile(_ < 5).toList}")
  println(s"Stream(1,2,3,4,5).takeWhile(_ <= 5).toList = ${ss.takeWhile(_ <= 5).toList}")
  println(s"Stream(1,2,3,4,5).takeWhile(_ => false).toList = ${ss.takeWhile(_ => false).toList}")
  println("------------------------------------------------------------")
  println(s"Stream(1,2,3,4,5).filter(_ % 2 == 0).toList = ${ss.filter(_ % 2 == 0).toList}")
  println("------------------------------------------------------------")
  println(s"Stream(1,2,3,4) = ${Stream(1, 2, 3, 4)}")
  println(s"Stream(1,2,3,4).map(_ + 10) = ${Stream(1, 2, 3, 4).map(_ + 10)}")
  println(s"Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0) = ${Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0)}")
  println(s"Stream(1,2,3,4).map(_ + 10).filter(_ % 2 == 0).toList = ${Stream(1, 2, 3, 4).map(_ + 10).filter(_ % 2 == 0).toList}")
  println("------------------------------------------------------------")
  println(s"Stream.constant(5).take(3).toList = ${Stream.constant(5).take(3).toList}")
  println("------------------------------------------------------------")
  println(s"Stream.fibs.take(5).toList = ${Stream.fibs.take(5).toList}")
  println("------------------------------------------------------------")
  println(s"Stream.fibsU.take(5).toList = ${Stream.fibsU.take(5).toList}")
  println("------------------------------------------------------------")
  println(s"Stream.onesU.take(5).toList = ${Stream.onesU.take(5).toList}")
  println("------------------------------------------------------------")
  println(s" = ${}")
  println(s" = ${}")

}