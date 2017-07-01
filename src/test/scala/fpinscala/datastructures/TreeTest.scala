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

  test("depth") {
    assert(1 == Tree.depth(Leaf(1)))
    assert(2 == Tree.depth(Branch(Leaf(1), Leaf(5))))
    assert(3 == Tree.depth(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3))))
  }

  test("map") {
    assert(Leaf(2) == Tree.map(Leaf(1))(_ + 1))
    assert(Branch(Leaf(2), Leaf(10)) == Tree.map(Branch(Leaf(1), Leaf(5)))(_ * 2))
    assert(Branch(Branch(Leaf("7"), Leaf("2")), Leaf("3")) == Tree.map(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3)))(_.toString))
  }

  test("sizeViaFold") {
    assert(1 == Tree.sizeViaFold(Leaf()))
    assert(3 == Tree.sizeViaFold(Branch(Leaf(), Leaf())))
    assert(5 == Tree.sizeViaFold(Branch(Branch(Leaf(), Leaf()), Leaf())))
  }

  test("maximumViaFold") {
    assert(1 == Tree.maximumViaFold(Leaf(1)))
    assert(5 == Tree.maximumViaFold(Branch(Leaf(1), Leaf(5))))
    assert(7 == Tree.maximumViaFold(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3))))
  }

  test("depthViaFold") {
    assert(1 == Tree.depthViaFold(Leaf(1)))
    assert(2 == Tree.depthViaFold(Branch(Leaf(1), Leaf(5))))
    assert(3 == Tree.depthViaFold(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3))))
  }

  test("mapViaFold") {
    assert(Leaf(2) == Tree.mapViaFold(Leaf(1))(_ + 1))
    assert(Branch(Leaf(2), Leaf(10)) == Tree.mapViaFold(Branch(Leaf(1), Leaf(5)))(_ * 2))
    assert(Branch(Branch(Leaf("7"), Leaf("2")), Leaf("3")) == Tree.mapViaFold(Branch(Branch(Leaf(7), Leaf(2)), Leaf(3)))(_.toString))
  }

}
