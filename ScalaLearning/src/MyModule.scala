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


  def fibonacci(n: Int):Int ={
    @tailrec
    def go(n: Int, value1: Int, value2: Int): Int = {
      if(n<=1) value2
      else go(n-1,value2,value1+value2)
    }
    go(n,0,1)
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(MyModule abs -7)
    println(factorial(7))
    println(fibonacci(9))
  }
}
