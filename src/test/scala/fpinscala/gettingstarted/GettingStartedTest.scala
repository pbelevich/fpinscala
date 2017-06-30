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

  test("fib") {
    assert(0 == MyModule.fib(0))
    assert(1 == MyModule.fib(1))
    assert(1 == MyModule.fib(2))
    assert(2 == MyModule.fib(3))
    assert(3 == MyModule.fib(4))
    assert(5 == MyModule.fib(5))
    assert(8 == MyModule.fib(6))
  }

}
