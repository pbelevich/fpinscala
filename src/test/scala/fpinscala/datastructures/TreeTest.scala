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

}
