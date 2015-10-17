package com.perevillega.ch8

import com.perevillega.ch5.Stream
import com.perevillega.ch6.{SimpleRNG, RNG, State}
import com.perevillega.ch8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

// due to exploratory nature of chapter (evolving api) there are no tests associated

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(f(_).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(i => Gen.listOfN(i, this))

  def unsized: SGen[A] = SGen((a) => this)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    def boundaries(i: Int) = start + i % (stopExclusive - start)

    Gen(State(RNG.nonNegativeInt).map(boundaries))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence((0 until n).map(_ => g.sample).toList))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen.boolean.flatMap(b => if (b) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    Gen(State(RNG.double)).flatMap(d => if (d < g1._2) g1._1 else g2._1)

  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => listOfN(i, g))

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(i => listOfN(i max 1, g))
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (m, n, rng) =>
      this.run(m, n, rng) match {
        case f: Falsified => f
        case Passed => p.run(m, n, rng)
      }
  }

  def ||(p: Prop): Prop = Prop {
    (m, n, rng) =>
      this.run(m, n, rng) match {
        case f: Falsified => p.run(m, n, rng)
        case Passed => Passed
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1))/max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(i => g(i))(f))
      val prop: Prop = props.map(p => Prop {
        (max, _, rng) =>
          p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def run(p: Prop, maxSize: Int = 100, testCases: Int = 100, rng: RNG = SimpleRNG(System.currentTimeMillis())): (String, Boolean) =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          (s"! Falsified after $n passed tests: $msg", false)
        case Passed =>
          (s"+ OK, passed $testCases tests.", true)
      }
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => Gen[B]): SGen[B] =
    SGen(forSize andThen(g => g.flatMap(f)))


}

