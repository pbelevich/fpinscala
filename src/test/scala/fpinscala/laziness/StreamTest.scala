package fpinscala.laziness

import org.scalatest.FunSuite

/**
  * @author Pavel Belevich
  */
class StreamTest extends FunSuite {

  test("testToList") {
    assert(List() == Stream().toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).toList)
  }

  test("testToListRecursive") {
    assert(List() == Stream().toListRecursive)
    assert(List(1, 2, 3) == Stream(1, 2, 3).toListRecursive)
  }

  test("take") {
    assert(Stream() == Stream().take(0))
    assert(Stream() == Stream().take(1))
    assert(Stream() == Stream().take(2))
    assert(Stream() == Stream(1, 2, 3).take(0))
    assert(List(1) == Stream(1, 2, 3).take(1).toList)
    assert(List(1, 2) == Stream(1, 2, 3).take(2).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).take(3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).take(4).toList)
  }

  test("drop") {
    assert(Stream() == Stream().take(0))
    assert(Stream() == Stream().take(1))
    assert(Stream() == Stream().take(2))
    assert(Stream() == Stream(1, 2, 3).take(0))
    assert(List(1) == Stream(1, 2, 3).take(1).toList)
    assert(List(1, 2) == Stream(1, 2, 3).take(2).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).take(3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).take(4).toList)
  }

  test("takeWhile") {
    assert(List() == Stream[Int]().takeWhile(_ < 1).toList)
    assert(List() == Stream(1, 2, 3).takeWhile(_ < 1).toList)
    assert(List(1) == Stream(1, 2, 3).takeWhile(_ < 2).toList)
    assert(List(1, 2) == Stream(1, 2, 3).takeWhile(_ < 3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).takeWhile(_ < 4).toList)
  }

  test("exists") {
    assert(Stream(1, 2, 3).exists(_ == 2))
    assert(!Stream(1, 2, 3).exists(_ == 4))
  }

  test("foldRight") {
    assert("321" == Stream(1, 2, 3).foldRight("")((i, s) => s + i.toString))
  }

  test("existsViaFoldRight") {
    assert(Stream(1, 2, 3).existsViaFoldRight(_ == 2))
    assert(!Stream(1, 2, 3).existsViaFoldRight(_ == 4))
  }

  test("forAll") {
    assert(!Stream(1, 2, 3).forAll(_ < 3))
    assert(Stream(1, 2, 3).forAll(_ < 4))
  }

  test("takeWhileViaFoldRight") {
    assert(List() == Stream[Int]().takeWhileViaFoldRight(_ < 1).toList)
    assert(List() == Stream(1, 2, 3).takeWhileViaFoldRight(_ < 1).toList)
    assert(List(1) == Stream(1, 2, 3).takeWhileViaFoldRight(_ < 2).toList)
    assert(List(1, 2) == Stream(1, 2, 3).takeWhileViaFoldRight(_ < 3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).takeWhileViaFoldRight(_ < 4).toList)
  }

  test("headOptionViaFoldRight") {
    assert(None == Stream().headOptionViaFoldRight)
    assert(Some(1) == Stream(1, 2, 3).headOptionViaFoldRight)
  }

  test("map") {
    assert(List("1", "2", "3") == Stream(1, 2, 3).map(_.toString).toList)
  }

  test("append") {
    assert(List(1, 2, 3, 4, 5, 6) == (Stream(1, 2, 3) append Stream(4, 5, 6)).toList)
  }

  test("flatMap") {
    assert(List('a', 'b', 'c', 'd', 'e', 'f') == Stream("abc", "d", "ef").flatMap(s => Stream(s.toCharArray: _*)).toList)
  }

  test("filter") {
    assert(List(4, 6) == Stream(1, 3, 4, 5, 6).filter(_ % 2 == 0).toList)
  }

  test("find") {
    assert(Some(4) == Stream(1, 2, 3, 4, 5).find(_ == 4))
    assert(None == Stream(1, 2, 3, 4, 5).find(_ == 6))
  }

  test("ones") {
    assert(List(1, 1, 1, 1, 1) == Stream.ones.take(5).toList)
  }

  test("constant") {
    assert(List(42, 42, 42, 42, 42) == Stream.constant(42).take(5).toList)
  }

  test("from") {
    assert(List(42, 43, 44, 45, 46) == Stream.from(42).take(5).toList)
  }

  test("fibs") {
    assert(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) == Stream.fibs.take(10).toList)
  }

  test("unfold") {
    assert(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) == (Stream.unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) } take 10 toList))
  }

  test("fibsViaUnfold") {
    assert(List(0, 1, 1, 2, 3, 5, 8, 13, 21, 34) == (Stream.fibsViaUnfold take 10 toList))
  }

  test("fromViaUnfold") {
    assert(List(42, 43, 44, 45, 46) == Stream.fromViaUnfold(42).take(5).toList)
  }

  test("constantViaUnfold") {
    assert(List(42, 42, 42, 42, 42) == Stream.constantViaUnfold(42).take(5).toList)
  }

  test("onesViaUnfold") {
    assert(List(1, 1, 1, 1, 1) == Stream.onesViaUnfold.take(5).toList)
  }

  test("mapViaUnfold") {
    assert(List("1", "2", "3") == Stream(1, 2, 3).mapViaUnfold(_.toString).toList)
  }

  test("takeViaUnfold") {
    assert(Stream() == Stream().takeViaUnfold(0))
    assert(Stream() == Stream().takeViaUnfold(1))
    assert(Stream() == Stream().takeViaUnfold(2))
    assert(Stream() == Stream(1, 2, 3).takeViaUnfold(0))
    assert(List(1) == Stream(1, 2, 3).takeViaUnfold(1).toList)
    assert(List(1, 2) == Stream(1, 2, 3).takeViaUnfold(2).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).takeViaUnfold(3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).takeViaUnfold(4).toList)
  }

  test("takeWhileViaUnfold") {
    assert(List() == Stream[Int]().takeWhileViaUnfold(_ < 1).toList)
    assert(List() == Stream(1, 2, 3).takeWhileViaUnfold(_ < 1).toList)
    assert(List(1) == Stream(1, 2, 3).takeWhileViaUnfold(_ < 2).toList)
    assert(List(1, 2) == Stream(1, 2, 3).takeWhileViaUnfold(_ < 3).toList)
    assert(List(1, 2, 3) == Stream(1, 2, 3).takeWhileViaUnfold(_ < 4).toList)
  }

  test("zipWith") {
    assert(List(4, 10, 18) == Stream(1, 2, 3).zipWith(Stream(4, 5, 6))(_ * _).toList)
    assert(List() == Stream[Int]().zipWith(Stream(4, 5, 6))(_ * _).toList)
    assert(List() == Stream(1, 2, 3).zipWith(Stream[Int]())(_ * _).toList)
  }

  test("zip") {
    assert(List((1, 4), (2, 5), (3, 6)) == Stream(1, 2, 3).zip(Stream(4, 5, 6)).toList)
    assert(List() == Stream[Int]().zip(Stream(4, 5, 6)).toList)
    assert(List() == Stream(1, 2, 3).zip(Stream[Int]()).toList)
  }

  test("zipWithAll") {
    def mult(A: Option[Int], B: Option[Int]): Option[Int] = for {a <- A; b <- B} yield a * b

    def multOr42(a: Option[Int], b: Option[Int]): Int = mult(a, b).getOrElse(42)

    assert(List(4, 10, 18) == Stream(1, 2, 3).zipWithAll(Stream(4, 5, 6))(multOr42).toList)
    assert(List(42, 42, 42) == Stream[Int]().zipWithAll(Stream(4, 5, 6))(multOr42).toList)
    assert(List(42, 42, 42) == Stream(1, 2, 3).zipWithAll(Stream[Int]())(multOr42).toList)

    assert(List(4, 10, 42) == Stream(1, 2, 3).zipWithAll(Stream(4, 5))(multOr42).toList)
  }

  test("zipAll") {
    assert(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), Some(6))) == Stream(1, 2, 3).zipAll(Stream(4, 5, 6)).toList)
    assert(List((None, Some(4)), (None, Some(5)), (None, Some(6))) == Stream[Int]().zipAll(Stream(4, 5, 6)).toList)
    assert(List((Some(1), None), (Some(2), None), (Some(3), None)) == Stream(1, 2, 3).zipAll(Stream[Int]()).toList)
    assert(List((Some(1), Some(4)), (Some(2), Some(5)), (Some(3), None)) == Stream(1, 2, 3).zipAll(Stream(4, 5)).toList)
  }

  test("startsWith") {
    assert(Stream(1, 2, 3).startsWith(Stream()))
    assert(Stream(1, 2, 3).startsWith(Stream(1)))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2)))
    assert(Stream(1, 2, 3).startsWith(Stream(1, 2, 3)))
    assert(!Stream(1, 2, 3).startsWith(Stream(1, 2, 3, 4)))
    assert(!Stream(1, 2, 3).startsWith(Stream(2, 3, 4)))
    assert(!Stream(1, 2, 3).startsWith(Stream(3, 4)))
    assert(!Stream(1, 2, 3).startsWith(Stream(4)))
  }

  test("tails") {
    assert(List(List(1, 2, 3), List(2, 3), List(3), List()) == Stream(1, 2, 3).tails.mapViaUnfold(_.toList).toList)
  }

}
