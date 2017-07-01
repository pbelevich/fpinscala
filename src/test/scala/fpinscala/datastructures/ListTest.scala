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

  test("dropWhile") {
    val l = List(1, 2, 3)
    assert(List(1, 2, 3) == List.dropWhile[Int](l, _ < 1))
    assert(List(2, 3) == List.dropWhile[Int](l, _ < 2))
    assert(List(3) == List.dropWhile[Int](l, _ < 3))
    assert(List() == List.dropWhile[Int](l, _ < 4))
  }

  test("append") {
    assert(List(1, 2, 3, 4, 5, 6) == List.append(List(1, 2, 3), List(4, 5, 6)))
  }

  test("init") {
    assert(List(1, 2) == List.init(List(1, 2, 3)))
  }

  test("sum2") {
    assert(0 == List.sum2(List()))
    assert(1 == List.sum2(List(1)))
    assert(6 == List.sum2(List(1, 2, 3)))
  }

  test("product2") {
    assert(1 == List.product2(List()))
    assert(42 == List.product2(List(42)))
    assert(6 == List.product2(List(1, 2, 3)))
  }

  test("length") {
    assert(0 == List.length(List()))
    assert(1 == List.length(List(1)))
    assert(3 == List.length(List(1, 2, 3)))
  }

  test("sum3") {
    assert(0 == List.sum3(List()))
    assert(1 == List.sum3(List(1)))
    assert(6 == List.sum3(List(1, 2, 3)))
  }

  test("product3") {
    assert(1 == List.product3(List()))
    assert(42 == List.product3(List(42)))
    assert(6 == List.product3(List(1, 2, 3)))
  }

  test("length2") {
    assert(0 == List.length2(List()))
    assert(1 == List.length2(List(1)))
    assert(3 == List.length2(List(1, 2, 3)))
  }

  test("reverse") {
    assert(List() == List.reverse(List()))
    assert(List(1) == List.reverse(List(1)))
    assert(List(3, 2, 1) == List.reverse(List(1, 2, 3)))
  }

  test("foldRightViaFoldLeft") {
    assert("cba" == List.foldRightViaFoldLeft(List("a", "b", "c"), "")((x, r) => r + x))
  }

  test("foldRightViaFoldLeft_1") {
    assert("cba" == List.foldRightViaFoldLeft_1(List("a", "b", "c"), "")((x, r) => r + x))
  }

  test("foldLeftViaFoldRight") {
    assert("abc" == List.foldLeftViaFoldRight(List("a", "b", "c"), "")((r, x) => r + x))
  }

  test("append2") {
    assert(List(1, 2, 3, 4, 5, 6) == List.append2(List(1, 2, 3), List(4, 5, 6)))
  }

  test("concat") {
    assert(List(1, 2, 3, 4, 5, 6) == List.concat(List(List(1, 2), List(3), List(4, 5, 6))))
  }

  test("add1") {
    assert(List(2, 3, 4) == List.add1(List(1, 2, 3)))
  }

  test("doubleToString") {
    assert(List("1.0", "2.0", "3.0") == List.doubleToString(List(1.0, 2.0, 3.0)))
  }

  test("map") {
    assert(List("1.0", "2.0", "3.0") == List.map(List(1.0, 2.0, 3.0))(_.toString))
  }

  test("filter") {
    assert(List(2, 4) == List.filter(List(1, 2, 3, 4))(_ % 2 == 0))
  }

  test("flatMap") {
    assert(List('a', 'b', 'c', 'd', 'e', 'f') == List.flatMap(List("abc", "d", "ef"))(s => List(s.toCharArray: _*)))
  }

}
