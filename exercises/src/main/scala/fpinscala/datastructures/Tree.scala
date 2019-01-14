package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /* EXERCISE 3.25
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.*/
  def size[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right)
  }

  /* EXERCISE 3.26
   * Write a function maximum that returns the maximum element in a Tree[Int]. */
  def max(root: Tree[Int]): Int = {
    def go(sub: Tree[Int], m: Int): Int = sub match {
      case Leaf(i) => i.max(m)
      case Branch(left, right) => m.max(go(left, m)).max(go(right, m))
    }

    go(root, Int.MinValue)
  }


  /* EXERCISE 3.27
   * Write a function depth that returns the maximum path length from the root of a tree to any leaf. */
  def depth[A](root: Tree[A]): Int = root match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  /* EXERCISE 3.28
   * Write a function map, analogous to the method of the same name on List,
   * that modifies each element in a tree with a given function. */
  def map[A, B](root: Tree[A])(f: A => B): Tree[B] = root match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }


  /* EXERCISE 3.29
 * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
 * Reimplement them in terms of this more general function.
 * Can you draw an analogy between this fold function and the left and right folds for List? */
  def fold[A, B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size2[A](root: Tree[A]): Int = fold[A, Int](root)(_ => 1)(_ + _)

  def max2(root: Tree[Int]): Int = fold[Int, Int](root)(identity)(_.max(_))

  def depth2[A](root: Tree[A]): Int = fold[A, Int](root)(_ => 0)(_.max(_) + 1)

  def map2[A, B](root: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](root)(f andThen Leaf.apply)(Branch.apply)

}

object TestTree {

  def main(args: Array[String]): Unit = {
    val testTree =
      Branch(
        Branch(
          Leaf(1),
          Branch(
            Leaf(2),
            Branch(
              Leaf(3),
              Leaf(4)))),
        Branch(
          Leaf(5),
          Leaf(6)))

    println(s"Tree.size($testTree) = ${Tree.size(testTree)}")
    println(s"Tree.max($testTree) = ${Tree.max(testTree)}")
    println(s"Tree.depth($testTree) = ${Tree.depth(testTree)}")
    println(s"Tree.map($testTree)(_ + 1) = ${Tree.map(testTree)(_ + 1)}")

    println(s"Tree.size2($testTree) = ${Tree.size2(testTree)}")
    println(s"Tree.max2($testTree) = ${Tree.max2(testTree)}")
    println(s"Tree.depth2($testTree) = ${Tree.depth2(testTree)}")
    println(s"Tree.map2($testTree)(_ + 1) = ${Tree.map2(testTree)(_ + 1)}")
  }
}
