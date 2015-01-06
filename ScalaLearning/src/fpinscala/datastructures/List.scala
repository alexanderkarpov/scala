package fpinscala.datastructures

sealed trait List[+A] //A trait is an abstract interface
// that may optionally contain implementations of some methods.
//Adding sealed in front means that all implementations of the trait
// must be declared in this file.

//There are two such implementations, or data constructors, of List (each introduced
//with the keyword case ) declared next, to represent the two possible forms a List can
//take.
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
 * Singly linked list
 */
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil

  }

}
