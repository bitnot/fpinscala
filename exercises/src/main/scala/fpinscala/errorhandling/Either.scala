package fpinscala.errorhandling


import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  // 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(b) => Right(f(b))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(b) => f(b)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(_) => this
    case _ => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Right(x), Right(y)) => Right(f(x, y))
    case (Left(x), _) => Left(x)
    case (_, Left(x)) => Left(x)
  }
}

case class Left[+E](get: E) extends Either[E, Nothing]

case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  // 4.7
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
    case Nil => Right(Nil)
    case h :: t => f(h) match {
      case l@Left(_) => l
      case Right(x) => traverse(t)(f).map(xs => x :: xs)
    }
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}

// 4.8
object Validation {

  sealed trait ValidatedList[+E, +A] {
    def map[B](f: A => B): ValidatedList[E, B] = this match {
      case i@Invalid(error) => i
      case Valid(result) => Valid(f(result))
    }

    def map2[EE >: E, B, C](other: ValidatedList[EE, B])(f: (A, B) => C): ValidatedList[EE, C] = (this, other) match {
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (Invalid(errorsA), Invalid(errorsB)) => Invalid(errorsA ++ errorsB)
      case (i@Invalid(errors), _) => i
      case (_, i@Invalid(errors)) => i
    }
  }

  def valid[E, A](a: A): ValidatedList[E, A] = Valid(a)

  def invalid[E, A](errors: List[E]): ValidatedList[E, A] = Invalid(errors)

  case class Invalid[+E](errors: List[E]) extends ValidatedList[E, Nothing]

  case class Valid[+A](result: A) extends ValidatedList[Nothing, A]

  def main(args: Array[String]): Unit = {
    val v1: ValidatedList[String, Int] = valid(1)
    val v5: ValidatedList[String, Int] = valid(5)
    val i1: ValidatedList[String, Int] = invalid(List("error1"))
    val i2: ValidatedList[String, Int] = invalid(List("error2"))
    println(s"... = ${v5.map2(v1)(_ + _)}")
    println(s"... = ${v5.map2(i1)(_ + _)}")
    println(s"... = ${i2.map2(i1)(_ + _)}")
    println(s"... = ${i2.map2(v1)(_ + _)}")
  }

}