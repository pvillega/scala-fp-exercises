package com.perevillega.ch6

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

object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (x, next) = rng.nextInt
    val res = if (x > 0) x else -(x + 1)
    (res, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (x, next) = nonNegativeInt(rng)
    val res = x / (Int.MaxValue.toLong + 1)
    (res.toDouble, next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (d1, rng1) = rng.nextInt
    val (i1, rng2) = rng1.nextInt
    ((i1, d1), rng2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val res = intDouble(rng)
    (res._1.swap, res._2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count < 1)
      (Nil, rng)
    else {
      val (i, r) = rng.nextInt
      val loop = ints(count - 1)(r)
      (i :: loop._1, loop._2)
    }

  type Rand[+A] = RNG => (A, RNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  def doubleUsingMap(rng: RNG): Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rna) = ra(rng)
    val (b, rnb) = rb(rna)
    (f(a, b), rnb)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(_.nextInt, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, _.nextInt)

  // remember what map2 does, you work in the function with the internal elements (a, b) not with the Rand!
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val emptyRng = unit(List[A]())
    val result = fs.foldRight(emptyRng)((f, acc) => map2(f, acc)((a, b) => a :: b))
    result
  }

  // the hint of using List.fill and changing the signature help a lot
  def intWithSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(_.nextInt))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rna) = f(rng)
    g(a)(rna)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    def checkSkew = (i: Int) =>
      if (i + (n - 1) - (i % n) >= 0) unit(i % n)
      else nonNegativeLessThan(n)

    flatMap(nonNegativeInt)(checkSkew)
  }

  def mapUsingFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2UsingFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    def g = (a: A) => map(rb)(b => f(a, b))
    flatMap(ra)(g)
  }

}

case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = State(s => {
    val (a, state2) = run(s)
    (f(a), state2)
  })

  def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = State(st => {
    val (a, rna) = run(st)
    val (b, rnb) = rb.run(rna)
    (f(a, b), rnb)
  })

  def flatMap[B](g: A => State[S, B]): State[S, B] = State(st => {
    val (a, rna) = run(st)
    g(a).run(rna)
  })
}

object State {
  // type only used if we don't have the class
  // type State[S,+A] = S => (A, S)

  def unit[S, A](a: A): State[S, A] = State(st => (a, st))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] = {
    val emptyRng = unit[S, List[A]](List())
    val result = fs.foldRight(emptyRng)((f, acc) => f.map2(acc)((a, b) => a :: b))
    result
  }
}


sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def apply(coins: Int, candies: Int) = new Machine(true, candies, coins)

  private def hasCandy(m: Machine) = m.candies > 0

  private def insertCoin(m: Machine): Machine =
    if (!hasCandy(m) || !m.locked) m
    else Machine(locked = false, m.candies, m.coins + 1)

  private def turnKnob(m: Machine): Machine =
    if (!hasCandy(m) || m.locked) m
    else Machine(locked = true, m.candies - 1, m.coins)

  private def nextState(i: Input, m: Machine) =
    i match {
      case Coin => insertCoin(m)
      case Turn => turnKnob(m)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(m => {
    val finalState = inputs.foldLeft(m)((st, input) => nextState(input, st))
    ((finalState.coins, finalState.candies), finalState)
  })

}
