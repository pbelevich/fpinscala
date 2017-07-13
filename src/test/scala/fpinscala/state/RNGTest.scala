package fpinscala.state

import fpinscala.state.RNG.Rand
import org.scalatest.FunSuite

import scala.collection.Searching.search

/**
  * @author Pavel Belevich
  */
class RNGTest extends FunSuite {

  private val maxPlus1 = Int.MaxValue.toDouble + 1

  test("testNextInt") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = rng.nextInt
    assert(n1 == 16159453)
    assert(rng2 != null)
    val (n2, rng3) = rng2.nextInt
    assert(n2 == -1281479697)
    assert(rng3 != null)
  }

  test("testRandomPair") {
    val rng = SimpleRNG(42)
    val ((n1, n2), rng3) = RNG.randomPair(rng)
    assert(n1 == 16159453)
    assert(n2 == -1281479697)
    assert(rng3 != null)
  }

  test("testNonNegativeInt") {
    val rng = SimpleRNG(42)
    val (n1, rng2) = RNG.nonNegativeInt(rng)
    assert(n1 == 16159453)
    assert(rng2 != null)
    val (n2, rng3) = RNG.nonNegativeInt(rng2)
    assert(n2 == 1281479697)
    assert(rng3 != null)
  }

  test("testDouble") {
    val rng = SimpleRNG(42)
    val (d1, rng2) = RNG.double(rng)
    assert(d1 == 16159453.0 / maxPlus1)
    assert(rng2 != null)
    val (d2, rng3) = RNG.double(rng2)
    assert(d2 == 1281479697.0 / maxPlus1)
    assert(rng3 != null)
  }

  test("testIntDouble") {
    val rng = SimpleRNG(42)
    val ((i, d), rng2) = RNG.intDouble(rng)
    assert(i == 16159453)
    assert(d == 1281479697.0 / maxPlus1)
    assert(rng2 != null)
  }

  test("testDoubleInt") {
    val rng = SimpleRNG(42)
    val ((d, i), rng2) = RNG.doubleInt(rng)
    assert(d == 1281479697.0 / maxPlus1)
    assert(i == 16159453)
    assert(rng2 != null)
  }

  test("testDouble3") {
    val rng = SimpleRNG(42)
    val ((d1, d2, d3), rng2) = RNG.double3(rng)
    assert(d1 == 16159453.0 / maxPlus1)
    assert(d2 == 1281479697.0 / maxPlus1)
    assert(d3 == 340305902.0 / maxPlus1)
    assert(rng2 != null)
  }

  test("testInts") {
    val rng = SimpleRNG(42)
    val (ints, rng2) = RNG.ints(3)(rng)
    assert(ints == List(-340305902, -1281479697, 16159453))
    assert(rng2 != null)
  }

  test("testInts2") {
    val rng = SimpleRNG(42)
    val (ints, rng2) = RNG.ints2(3)(rng)
    assert(ints == List(-340305902, -1281479697, 16159453))
    assert(rng2 != null)
  }

  test("testNonNegativeEven") {
    val rng = SimpleRNG(42)
    val (i, rng2) = RNG.nonNegativeEven(rng)
    assert(i % 2 == 0)
    assert(rng2 != null)
  }

  test("testDoubleViaMap") {
    val rng = SimpleRNG(42)
    val (d1, rng2) = RNG.doubleViaMap(rng)
    assert(d1 == 16159453.0 / maxPlus1)
    assert(rng2 != null)
    val (d2, rng3) = RNG.doubleViaMap(rng2)
    assert(d2 == 1281479697.0 / maxPlus1)
    assert(rng3 != null)
  }

}
