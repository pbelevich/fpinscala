package fpinscala.state

import org.scalatest.FunSuite

/**
  * @author Pavel Belevich
  */
class RNGTest extends FunSuite {

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
    assert(d1 == 16159453.0 / Int.MaxValue)
    assert(rng2 != null)
    val (d2, rng3) = RNG.double(rng2)
    assert(d2 == 1281479697.0 / Int.MaxValue)
    assert(rng3 != null)
  }

}
