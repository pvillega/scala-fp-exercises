package com.perevillega.ch7

import java.util.concurrent._
import java.util.concurrent.atomic.AtomicReference

import com.perevillega.ch7.Par.Par

object Par {
  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isDone: Boolean = true
  }

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](a: => Par[A]): Par[A] =
    (es: ExecutorService) =>
      es.submit(new Callable[A] {
        def call = a(es).get
      })

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def noOp = unit(())

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = map2(pa, noOp) { (a, _) => f(a) }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(ps.map(asyncF(f)))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
    val doubleList = as.map(asyncF(a => if (f(a)) List(a) else Nil))
    map(sequence(doubleList))(l => l.flatten)
  }

  def delay[A](a: => Par[A]): Par[A] = es => a(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es => if (run(es)(cond).get) t(es) else f(es)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => {
      val i = run(es)(n).get
      choices(i)(es)
    }

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val select: Par[Int] = map(cond)(b => if (b) 0 else 1)
    choiceN(select)(t :: f :: Nil)
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => {
      val k = run(es)(key).get
      choices(k)(es)
    }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val c = run(es)(pa).get
      choices(c)(es)
    }

  def choice3[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(b => if (b) t else f)

  def choiceN_2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(i => choices(i))

  def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get())

  def flatMap[A,B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def joinByFlatMap[A](a: Par[Par[A]]): Par[A] = flatMap(a)(p => p)

}

object Actors {

  sealed trait Future[A] {
    private[ch7] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) {
      a =>
        ref.set(a)
        latch.countDown()
    }
    latch.await()
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      override private[ch7] def apply(k: (A) => Unit): Unit = k(a)
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      def call = r
    })

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      override private[ch7] def apply(k: (A) => Unit): Unit =
        eval(es)(a(es)(k))
    }

  // Actors were removed form Scala standard lib
  //  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] =
  //    es => new Future[C] {
  //      override private[ch7] def apply(k: (C) => Unit): Unit = {
  //        var ar: Option[A] = None
  //        var br: Option[B] = None
  //
  //        val combiner = scala.Actor[Either[A,B]](es) {
  //          case Left(a) => br match {
  //            case None => ar = Some(a)
  //            case Some(b) => eval(es)(k(f(a,b)))
  //          }
  //          case Right(b) => ar match {
  //            case None => br = Some(b)
  //            case Some(a) => eval(es)(k(f(a,b)))
  //          }
  //        }
  //
  //        p(es)(a => combiner ! Left(a))
  //        p2(es)(b => combiner ! Right(b))
  //      }
  //    }
}

object Example {
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption.getOrElse(0))
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

}
