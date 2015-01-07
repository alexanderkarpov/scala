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

  //List.apply("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
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

  /**
   * EXERCISE 3.4
   * Generalize tail to the function drop , which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being
   * dropped — we don’t need to make a copy of the entire List .
   * @param l list
   * @param n number of elements to drop
   * @tparam A list element type
   * @return the list without n first elements
   */
  def drop[A](l:List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) =>
      if (n<=0) l
      else drop(xs, n-1)
  }

  /**
   * EXERCISE 3.5
   * Implement dropWhile , which removes elements from the List prefix as long as
   * they match a predicate.
   * @param l list
   * @param f match function
   * @tparam A list element type
   * @return result list
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match{
    case Nil => Nil
    case Cons(h,t) if f(h) => dropWhile(t,f)
    case _ => l

  }


  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h,append(t,a2))
  }

  /**
   * EXERCISE 3.6
   * Returns a List consisting of all but the last element of a List .
   * So, given List(1,2,3,4), init will return List(1,2,3)
   * @param l given list
   * @tparam A list element type
   * @return a List consisting of all but the last element of a given List
   */
  def init[A] (l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }

}
