package fpinscala.datastructures

/**
 * Singly linked list
 * @tparam A list element type
 */
sealed trait List[+A] //A trait is an abstract interface
// that may optionally contain implementations of some methods.
//Adding sealed in front means that all implementations of the trait
// must be declared in this file.

//There are two such implementations, or data constructors, of List (each introduced
//with the keyword case ) declared next, to represent the two possible forms a List can
//take.
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
// [+A] - the + indicates that the type parameter A is covariant
// (for all types X and Y, if X is a subtype of Y, then List[X]
// is a subtype of List[Y])

/**
 * companion object to List
 */
object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
      //If multiple patterns match the target,
      // Scala chooses the first matching case.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0 //the product of any list starting with 0.0 is 0.0
    case Cons(x,xs) => x * product(xs)

  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  /**
   * EXERCISE 3.2
   * Implement the function tail for removing the first element of a List .
   * @param as list
   * @tparam A list element type
   * @return tail of the list
   */
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  /**
   * EXERCISE 3.3
   * Using the same idea, implement the function setHead for replacing the first element
   * of a List with a different value.
   * @param as list
   * @param nv new value
   * @tparam A list element type
   * @return the list with replaced first element
   */
  def setHead[A](as: List[A], nv: A): List[A] = as match {
    case Nil => Nil
    case Cons(x,xs) => Cons(nv,xs)
  }

//  def main(args: Array[String]): Unit ={
//    val ex1: List[Double] = Nil
//    val ex2: List[Int] = Cons(1, Nil)
//    val ex3: List[String] = Cons("a", Cons("b", Nil))
//
//
//  }

}
