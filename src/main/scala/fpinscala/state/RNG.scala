package fpinscala.state

import scala.annotation.tailrec

/**
  * @author Pavel Belevich
  */
trait RNG {

  def nextInt: (Int, RNG)

}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

case object RNG {

  def randomPair(rng: RNG): ((Int, Int), RNG) = {
    val (i1, rng2) = rng.nextInt
    val (i2, rng3) = rng2.nextInt
    ((i1, i2), rng3)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = rng.nextInt match {
    case (Int.MinValue, rng2) => nonNegativeInt(rng2)
    case (i, rng2) if i < 0 => (-i, rng2)
    case (i, rng2) => (i, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, rng2) = nonNegativeInt(rng)
    (i.toDouble / (Int.MaxValue.toDouble + 1), rng2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0) {
      (Nil, rng)
    } else {
      val (t, rng2) = ints(count - 1)(rng)
      val (h, rng3) = rng2.nextInt
      (h :: t, rng3)
    }
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def loop(count: Int, rng: RNG, list: List[Int]): (List[Int], RNG) = {
      if (count == 0) {
        (list, rng)
      } else {
        val (i, rng2) = rng.nextInt
        loop(count - 1, rng2, i :: list)
      }
    }

    loop(count, rng, List())
  }

  case class State[S, +A](run: S => (A, S)) {

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

  }

  object State {

    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequenceViaFoldRight[S, A](sas: List[State[S, A]]): State[S, List[A]] =
      sas.foldRight[State[S, List[A]]](unit(List()))((a, b) => a.map2(b)(_ :: _))

  }

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  //  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def unit[A](a: A): Rand[A] = State.unit(a)

  //  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  //    val (a, rng2) = s(rng)
  //    (f(a), rng2)
  //  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = s map f

  def nonNegativeEven: Rand[Int] = map(State(nonNegativeInt))(i => i - i % 2)

  def doubleViaMap: Rand[Double] = map(State(nonNegativeInt))(_ / (Int.MaxValue.toDouble + 1))

  //  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  //    val (a, rng2) = ra(rng)
  //    val (b, rng3) = rb(rng2)
  //    (f(a, b), rng3)
  //  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = ra.map2(rb)(f)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, State(double))

  val randDoubleInt: Rand[(Double, Int)] = both(State(double), int)

  //  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  //    fs.foldRight[Rand[List[A]]](unit(List()))((a, b) => map2(a, b)(_ :: _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    State.sequenceViaFoldRight(fs)

  //  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  //    val (a, rng2) = f(rng)
  //    g(a)(rng2)
  //  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = f flatMap g

  def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

}
