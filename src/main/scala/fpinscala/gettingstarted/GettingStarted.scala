package fpinscala.gettingstarted

import scala.annotation.tailrec

/**
  * @author Pavel Belevich
  */
object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(n: Int, acc: Int): Int = if (n <= 0) acc else go(n - 1, n * acc)

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @tailrec
    def fib(a: Int, b: Int, k: Int): Int = {
      if (k == n) a + b else fib(b, a + b, k + 1)
    }

    if (n < 2) n else fib(0, 1, 2)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def isSorted(i: Int): Boolean =
      if (i >= as.length) true else ordered(as(i - 1), as(i)) && isSorted(i + 1)

    isSorted(1)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}