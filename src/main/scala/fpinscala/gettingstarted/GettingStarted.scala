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

}