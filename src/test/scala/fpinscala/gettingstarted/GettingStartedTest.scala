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

  test("factorial") {
    assert(1 == MyModule.factorial(1))
    assert(2 == MyModule.factorial(2))
    assert(6 == MyModule.factorial(3))
    assert(24 == MyModule.factorial(4))
    assert(120 == MyModule.factorial(5))
  }

}
