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
  }

}
