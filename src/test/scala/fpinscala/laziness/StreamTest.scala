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

}
