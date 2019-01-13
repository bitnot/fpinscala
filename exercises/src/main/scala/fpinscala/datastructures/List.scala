package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    ints match { // A function that uses pattern matching to add up a list of integers
      case Nil => 0 // The sum of the empty list is 0.
      case Cons(x, xs) =>
        x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
    }

  def product(ds: List[Double]): Double = ds match {
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil        => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  /*
  *  EXERCISE 3.7 -
  *  Can product, implemented using foldRight, immediately halt the recursion and return 0.0 if it encounters a 0.0?
  *  No, `List` will be unwrapped onto method stack before `f` is called, so there is no way to pass any
  *  circuit shortening logic to `foldRight`
  *
  *  EXERCISE 3.8
  *  `foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))`
  *  `Cons(1,Cons(2,Cons(3,Nil)))`
  *  Expression creates a copy of the list:
  *  `foldRight(List(1,2,3), Nil:List[Int])((el:Int, acc:List[Int]) => Cons(el, acc))`
  * */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil         => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil         => throw new Exception("list is empty")
    case Cons(_, hs) => hs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil         => throw new Exception("cannot set head on an empty list")
    case Cons(_, hs) => Cons(h, hs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (_, 0) => l
    case (Cons(_, hs), x) => drop(hs, x - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, hs) if f(h) => dropWhile(hs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = {
    @tailrec
    def go(acc: List[A], ls:List[A]):List[A] = ls match {
      case Nil => Nil
      case Cons(h1, Nil) => acc
      case Cons(h1, hs) => go(append(acc, Cons(h1, Nil)), hs)
    }
    go(Nil, l)
  }

  /*
  *  EXERCISE 3.9 - Compute the length of a list using foldRight
  * */
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, len) => 1 + len)

  /*
  *  EXERCISE 3.10
  * */
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
    @tailrec
    def go(rest: List[A], acc: B):B = rest match {
      case Nil => acc
      case Cons(h, hs) => go(hs, f(acc, h))
    }
    go(l, z)
  }

  def prepend[A](hs:List[A])(h:A) = Cons(h, hs)

  /*
  *  EXERCISE 3.11 - Write sum, product, and a function to compute the length of a list using foldLeft.
  * */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])(prepend(_)(_))

  /*
  *  EXERCISE 3.12
  *  It's possible to reverse a list using foldLeft and then traverse it again using foldLeft to get foldRight
  *  in constant time O(n)
  * */
  def foldRightL[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b,a) => f(a,b))
  /*
  * foldLeft can be implemented with foldRight, but it won't be stack safe and it will run in O(n^2) time
  * */
  def appendOneRight[A](h:A, hs:List[A]):List[A] = foldRight(hs, List(h))(Cons(_, _))
  def reverseRight[A](l: List[A]): List[A] = foldRight(l, Nil:List[A])(appendOneRight)
  def foldLeftR[A, B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverseRight(l), z)((a, b) => f(b,a))

  /* EXERCISE 3.14 - Implement append in terms of either foldLeft or foldRight.
  * see appendOneRight
  * */
  def appendOneRightL[A](xs:List[A], x:A):List[A] = foldLeft(reverse(xs), List(x))(prepend(_)(_))
  def appendRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))
  def append2[A](a1: List[A], a2: List[A]): List[A] = foldLeft(reverse(a1), a2)(prepend(_)(_))


  /* EXERCISE 3.15 - Write a function that concatenates a list of lists into a single list. (in linear time)
  * */
  def concat[A](ll: List[List[A]]):List[A] = foldLeft(reverse(ll), Nil:List[A])((acc, l) => append2(l, acc))

  /*
  * EXERCISE 3.16
  * */
  def plusOne(l:List[Int]):List[Int] = foldRight(l, Nil:List[Int])((h, hs) => Cons(h + 1, hs))
  /*
  * EXERCISE 3.17
  * */
  def print(l:List[Double]):List[String] = foldRight(l, Nil:List[String])((h, hs) => Cons(h.toString, hs))
  /*
  * EXERCISE 3.18
  * */
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, hs) => Cons(f(h), hs))
  def mapL[A, B](l: List[A])(f: A => B): List[B] = foldLeft(reverse(l), Nil:List[B])((hs, h) => Cons(f(h), hs))

  /*
  * EXERCISE 3.19
  * */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((h, hs) =>
    if(f(h)) Cons(h, hs)
    else hs )

  /*
  * EXERCISE 3.20
  scala> flatMap(l)(x => List((1 to x):_*))
  res0: fpinscala.datastructures.List[Int] = Cons(1,Cons(1,Cons(2,Cons(1,Cons(2,Cons(3,Nil))))))
  * */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  /*
  * EXERCISE 3.21 - Use flatMap to implement filter.
  * */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if(f(x)) List(x) else Nil)


  /*
  * EXERCISE 3.23 - a function that accepts two lists and constructs a new list by adding correspond- ing elements
  * */
  def addLists(as:List[Int], bs:List[Int]): List[Int] = (as, bs) match {
    case (Nil, _)             => Nil
    case (_, Nil)             => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons[Int](h1 + h2, addLists(t1, t2))
  }


  def zipWith[A,B,C](as:List[A], bs:List[B])(f: (A,B) => C): List[C] = (as, bs) match {
    case (Nil, _)             => Nil
    case (_, Nil)             => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }


  /**
    * EXERCISE 3.24
    * O(n^2)
    *
    */
  def allTrue(l:List[Boolean]): Boolean = foldLeft(l, true)(_ && _)
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil)             => true
    case (Nil, _)             => false
    case (l1@Cons(h1, t1),l2) => allTrue(zipWith(sup, sub)(_ == _)) || hasSubsequence(t1, sub)
  }
}


object TestList {
  import List._
  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _)))          => x
      case Nil                                   => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t)                            => h + sum(t)
      case _                                     => 101
    }
    println("""val x = List(1,2,3,4,5) match {
              |  case Cons(x, Cons(2, Cons(4, _))) => x
              |  case Nil => 42
              |  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
              |  case Cons(h, t) => h + sum(t)
              |  case _ => 101
              |}""".stripMargin)
    println(s"x = $x")

    println(s"List.tail(List(1,2,3)) = ${List.tail(List(1,2,3))}")
    println(s"List.setHead(List(1,2,3), 0) = ${List.setHead(List(1,2,3), 0)}")
    println(s"List.drop(List(1,2,3), 2) = ${List.drop(List(1,2,3), 2)}")
    println(s"List.dropWhile(List(1,2,3), (x:Int) => x < 2) = ${List.dropWhile(List(1,2,3), (x:Int) => x < 2)}")
    println(s"List.init(List(1,2,3)) = ${List.init(List(1,2,3))}")

    println(s"List.hasSubsequence(List(1,2,3),List(1,2,3)) = ${List.hasSubsequence(List(1,2,3),List(1,2,3))}")
    println(s"List.hasSubsequence(List(1,2,3),List(2,3)) = ${List.hasSubsequence(List(1,2,3),List(2,3))}")
    println(s"List.hasSubsequence(List(1,2,3),List(2)) = ${List.hasSubsequence(List(1,2,3),List(2))}")
    println(s"List.hasSubsequence(List(1,2,3),List(4,5)) = ${List.hasSubsequence(List(1,2,3),List(4,5))}")
  }
}
