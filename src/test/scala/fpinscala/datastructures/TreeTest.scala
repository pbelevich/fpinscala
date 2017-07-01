package fpinscala.datastructures

import org.scalatest.FunSuite

/**
  * @author Pavel Belevich
  */
class TreeTest extends FunSuite {

  test("size") {
    assert(1 == Tree.size(Leaf()))
    assert(3 == Tree.size(Branch(Leaf(), Leaf())))
    assert(5 == Tree.size(Branch(Branch(Leaf(), Leaf()), Leaf())))
  }

  test("maximum") {
    assert(1 == Tree.maximum(Leaf(1)))
    assert(5 == Tree.maximum(Branch(Leaf(1), Leaf(5))))
    assert(7 == Tree.maximum(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3))))
  }

}
