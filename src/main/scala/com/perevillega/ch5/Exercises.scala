package com.perevillega.ch5

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // potential stackoverflow on big lists!
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  // copied from book solutions, a tail rec implementation of toList
  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def skipAndBuild(n: Int, acc: Stream[A]): Stream[A] = acc match {
      case Empty => Empty
      case Cons(h, t) if n <= 0 => acc
      case Cons(h, t) if n > 0 => skipAndBuild(n - 1, t())
    }

    skipAndBuild(n, this)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    def buildWhileTrue(acc: Stream[A]): Stream[A] = acc match {
      case Cons(h, t) if p(h()) => Stream.cons(h(), buildWhileTrue(t()))
      case _ => Stream.empty
    }

    buildWhileTrue(this)
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhileFR(p: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  def headOptionFR: Option[A] = foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(Stream.empty[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((a, b) => Stream.cons(a, b))

  def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
    case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
    case _ => None
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = Stream.unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = Stream.unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean = this.zipAll(s)
    .takeWhile(_._2.isDefined)
    .forAll(pair => pair._1 == pair._2)

  def tails: Stream[Stream[A]] = {
    val tailStreams = Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) => Some((Cons(h, t): Stream[A], t()))
    }
    tailStreams append Stream(Empty)
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = Cons(() => n, () => from(n + 1))

  def fibs(): Stream[Int] = {
    def fibo(n: Int, m: Int): Stream[Int] = Cons(() => n, () => fibo(m, n + m))

    fibo(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Stream.empty[A]
      case Some((a, s)) => Cons(() => a, () => unfold(s)(f))
    }

  def fibsUnfold(): Stream[Int] = unfold((0,1))(f => Some(f._1, (f._2, f._1 + f._2)))

  def fromUnfold(n: Int): Stream[Int] = unfold(n)(f => Some(f, f+1))

  def constantUnfold[A](a: A): Stream[A] = unfold(a)(f => Some(f, f))
}

