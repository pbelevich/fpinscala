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

}
