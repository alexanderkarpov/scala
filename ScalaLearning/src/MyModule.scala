import fpinscala.datastructures.Cons

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int = {
    if (n<0) -n
    else n
  }

  private def formatAbs(x: Int): String = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, this.abs(x))
  }

  def factorial(n: Int):Long = {
    @tailrec
    def go(n: Int, acc: Long): Long = {
      if (n<=0) acc
      else go(n-1,n*acc)
    }
    go(n, 1)
  }

  // EXERCISE 2.1
  def fibonacci(n: Int):Int ={
    @tailrec
    def go(n: Int, value1: Int, value2: Int): Int = {
      if(n<=1) value2
      else go(n-1,value2,value1+value2)
    }
    go(n,0,1)
  }

  def formatResult(name: String, n: Int, f: Int=>Long) = {
    val msg = "The %s of %d is %d"
    msg.format(name,n,f(n))
  }

  def findFirst[A](as: Array[A], p: A=>Boolean) : Int = {
    def loop(n: Int): Int ={
      if(n>=as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    }
    loop(0)
  }

  // EXERCISE 2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean ={
      if(n>=as.length) true
      else if (!ordered(as(n-1),as(n))) false
      else loop(n+1)
    }
    loop(1)
  }

  //EXERCISE 2.3 (preparation)
  def partial1[A,B,C] (a: A,f: (A,B) => C): B =>C ={
    (b: B) => f(a,b)
  }

  //EXERCISE 2.3
  /*
  Let’s look at another example, currying, which converts a function f of two arguments
  into a function of one argument that partially applies f . Here again there’s only one
  implementation that compiles. Write this implementation.
   */
  def curry[A,B,C](f:(A,B) => C): A => (B => C) = {
    (a:A)=> (b:B) => f(a,b)
  }

  //EXERCISE 2.4
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = {
    (a:A, b:B) => f(a)(b)
  }

  //EXERCISE 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a:A) => f(g(a))
  }


  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(MyModule abs -7)

    println(factorial(7))
    println(fibonacci(9))

    println(formatResult("absolute value",9,abs))
    println(formatResult("factorial",8,factorial))
    println(formatResult("fibonacci",9,fibonacci))

    println(findFirst(Array(7,9,13), (x:Int) => x==9))
    println(findFirst(Array("A","B","CDE"), (x:String) => x=="CDE"))

    // EXERCISE 2.2 (preparation)
    val arr = Array(1,2,3,4,5,6,7)
    println("is sorted: "+isSorted(arr,(x:Int, y:Int) => y>x))

    //EXERCISE 2.3
    val c = partial1(35, (a: Int, b: Int) => a+b)
    val d = c(5)
    println(d)

    val e = partial1(35.5, (a: Double, b: Double) => a/b)
    val f = e(25.2)
    println(f)

    val list = fpinscala.datastructures.List.apply("A","B","C","D","E","F",
      "G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z")
    println(fpinscala.datastructures.List.drop(list,15))


    val xs: fpinscala.datastructures.List[Int] = fpinscala.datastructures.List(1,2,3,4,5,4,3,2,1)
    val ex1 = fpinscala.datastructures.List.dropWhile(xs)(x => x < 4)
    println(ex1)
  }
}
