package fpinscala.gettingstarted

/**
  * @author Pavel Belevich
  */
class GettingStartedTest extends org.scalatest.FunSuite {

  test("abs") {
    assert(42 == MyModule.abs(42))
    assert(0 == MyModule.abs(0))
    assert(42 == MyModule.abs(-42))
  }

}
