package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => this
    case _ => None
  }
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
    catch {
      case e: Exception => 43
    } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch {
      case e: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // 4.2
  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (Some(x), Some(y)) => Some(f(x, y))
    case _ => None
  }

  // 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case None :: _ => None
    case Some(a) :: tail =>
      sequence[A](tail) match {
        case None => None
        case Some(list) => Some(a :: list)
      }
  }

  // 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h) match {
      case None => None
      case Some(x) => traverse(t)(f).map(xs => x :: xs)
    }
  }
  def sequenceT[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(identity)
}

object OptionTest extends App {
  println(s"Option.sequence(List(Some(1), Some(2), Some(3))) = ${Option.sequence(List(Some(1), Some(2), Some(3)))}")
  println(s"Option.sequence(List(Some(1), None, Some(3))) = ${Option.sequence(List(Some(1), None, Some(3)))}")
  println(s"Option.sequence(List()) = ${Option.sequence(List())}")
  println(s"Option.traverse[Int, Int](List(1, 2, 3))(x => if (x > 1) Some(x) else None) = ${
    Option.traverse[Int, Int](List(1, 2, 3))(x => if (x > 1) Some(x) else None)
  }")
  println(s"Option.traverse[Int, Int](List(2, 3, 4))(x => if (x > 1) Some(x) else None) = ${
    Option.traverse[Int, Int](List(2, 3, 4))(x => if (x > 1) Some(x) else None)
  }")
}