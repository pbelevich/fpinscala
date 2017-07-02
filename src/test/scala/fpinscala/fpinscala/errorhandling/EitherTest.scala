package fpinscala.fpinscala.errorhandling

import org.scalatest.FunSuite

/**
  * @author Pavel Belevich
  */
class EitherTest extends FunSuite {

  test("testMap2") {
    assert(Left(1) == (Left(1): Either[Int, String]).map2(Left("2"): Either[String, Int])((s, i) => s.toInt + i))
    assert(Left(1) == (Left(1): Either[Int, String]).map2(Right(2): Either[String, Int])((s, i) => s.toInt + i))
    assert(Left("2") == (Right("1"): Either[Int, String]).map2(Left("2"): Either[String, Int])((s, i) => s.toInt + i))
    assert(Right(3) == (Right("1"): Either[Int, String]).map2(Right(2): Either[String, Int])((s, i) => s.toInt + i))
  }

  test("testOrElse") {
    assert(Left(2) == (Left(1): Either[Int, String]).orElse(Left(2)))
    assert(Right(2) == (Left(1): Either[Int, String]).orElse(Right(2)))
    assert(Right("1") == (Right("1"): Either[Int, String]).orElse(Left(2)))
    assert(Right("1") == (Right("1"): Either[Int, String]).orElse(Right(2)))
  }

  test("testFlatMap") {
    assert(Left(1) == (Left(1): Either[Int, String]).flatMap(_ => Left(2)))
    assert(Left(1) == (Left(1): Either[Int, String]).flatMap(_ => Right(2)))
    assert(Left(2) == (Right("1"): Either[Int, String]).flatMap(_ => Left(2)))
    assert(Right(2) == (Right("1"): Either[Int, String]).flatMap(_ => Right(2)))
  }

  test("testMap") {
    assert(Left(1) == (Left(1): Either[Int, String]).map(_.toInt))
    assert(Right(1) == (Right("1"): Either[Int, String]).map(_.toInt))
  }

}
