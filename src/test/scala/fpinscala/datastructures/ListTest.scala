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

}
