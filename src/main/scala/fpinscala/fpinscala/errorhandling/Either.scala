package fpinscala.fpinscala.errorhandling

/**
  * @author Pavel Belevich
  */

import scala.{Option => _, Either => _, Left => _, Right => _}

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  //    flatMap(x => b.flatMap(y => Right(f(x, y))))
    for {
      x <- this
      y <- b
    } yield f(x, y)

}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch {
      case e: Exception => Left(e)
    }
  }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

}