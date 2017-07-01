package fpinscala.datastructures

import org.scalatest.FunSuite

/**
  * @author Pavel Belevich
  */
class ListTest extends FunSuite {

  test("tail") {
    val l = List(1, 2, 3)
    assert(List(2, 3) == List.tail(l))
    assert(List(3) == List.tail(List.tail(l)))
    assert(List() == List.tail(List.tail(List.tail(l))))
  }

  test("setHead") {
    val l = List(1, 2, 3)
    assert(List(42, 2, 3) == List.setHead(l, 42))
  }

  test("drop") {
    val l = List(1, 2, 3)
    assert(List(1, 2, 3) == List.drop(l, 0))
    assert(List(2, 3) == List.drop(l, 1))
    assert(List(3) == List.drop(l, 2))
    assert(List() == List.drop(l, 3))
  }

}
