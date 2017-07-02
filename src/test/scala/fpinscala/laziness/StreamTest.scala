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

}
