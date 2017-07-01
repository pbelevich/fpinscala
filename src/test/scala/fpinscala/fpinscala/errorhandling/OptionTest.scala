package fpinscala.fpinscala.errorhandling

import org.scalatest.FunSuite

/**
  * @author Pavel Belevich
  */
class OptionTest extends FunSuite {

  test("testOrElse") {
    assert(Some(42) == (None: Option[Int]).orElse(Some(42)))
    assert(None == (None: Option[Int]).orElse(None))
    assert(Some(1) == (Some(1): Option[Int]).orElse(Some(42)))
    assert(Some(1) == (Some(1): Option[Int]).orElse(None))
  }

  test("testOrElse_1") {
    assert(Some(42) == (None: Option[Int]).orElse_1(Some(42)))
    assert(None == (None: Option[Int]).orElse_1(None))
    assert(Some(1) == (Some(1): Option[Int]).orElse_1(Some(42)))
    assert(Some(1) == (Some(1): Option[Int]).orElse_1(None))
  }

  test("testFilter") {
    assert(None == (None: Option[Int]).filter(_ % 2 == 0))
    assert(None == (Some(1): Option[Int]).filter(_ % 2 == 0))
    assert(Some(2) == (Some(2): Option[Int]).filter(_ % 2 == 0))
  }

  test("testFlatMap") {
    assert(None == (None: Option[Int]).flatMap(a => Some(a + 1)))
    assert(Some(2) == (Some(1): Option[Int]).flatMap(a => Some(a + 1)))
    assert(None == (Some(1): Option[Int]).flatMap(_ => None))
  }

  test("testFlatMap_1") {
    assert(None == (None: Option[Int]).flatMap_1(a => Some(a + 1)))
    assert(Some(2) == (Some(1): Option[Int]).flatMap_1(a => Some(a + 1)))
    assert(None == (Some(1): Option[Int]).flatMap_1(_ => None))
  }

  test("testGetOrElse") {
    assert(42 == (None: Option[Int]).getOrElse(42))
    assert(1 == (Some(1): Option[Int]).getOrElse(42))
  }

  test("testMap") {
    assert(None == (None: Option[Int]).map(_ + 1))
    assert(Some(2) == (Some(1): Option[Int]).map(_ + 1))
  }

  test("mean") {
    assert(None == Option.mean(Seq()))
    assert(Some(1.0) == Option.mean(Seq(1.0)))
    assert(Some(2.0) == Option.mean(Seq(1.0, 2.0, 3.0)))
  }

  test("variance") {
    assert(None == Option.variance(Seq()))
    assert(Some(0.0) == Option.variance(Seq(1.0)))
    assert(Some(5.0) == Option.variance(Seq(2.0, 4.0, 6.0, 8.0)))
  }

}
