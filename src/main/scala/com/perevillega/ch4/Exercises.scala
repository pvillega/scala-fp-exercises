package com.perevillega.ch4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(x) => f(x)
  }
  def getOrElse[B >: A](default: => B): B = this match {
    case None  => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(x) => Some(x)
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => Some(x)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A] 
case object None extends Option[Nothing] 

sealed trait Either[+E, +A] {  
  def map[B](f: A => B): Either[E,B] = this match {
    case Right(v) => Right(f(v))
    case Left(v) => Left(v)
  }
  // EE must be supertype of E to keep type consistency on the Error    
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => f(v)
    case Left(v) => Left(v)
  }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(v) => Right(v)
    case Left(_) => b
  }
  def map2[EE >: E, B, C](b: Either[EE,B])(f: (A,B) => C): Either[EE,C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Chapter4 {
  def Try[A](a: => A): Option[A] = try Some(a) catch { case e: Exception => None}
}

object Exercise42 {
  private def mean(xs: Seq[Double]): Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum/xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {    
    val m: Option[Double] = mean(xs)
    def f(rs: Seq[Double], m: Double): Option[Double] = mean(rs map (i => math.pow(i - m, 2)))
    m flatMap (mn => f(xs, mn))
  }
}

object Exercise43 {
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = a flatMap (i => b map (h => f(i,h)))
}

object Exercise44 {  
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t =>
      def concat(e: A, tail: List[Option[A]])= sequence(t) map (list => e :: list)
      h flatMap (e => concat(e, t))
  }
}

object Exercise45 {  
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => Exercise43.map2(f(h), traverse(t)(f))(_ :: _)
  }

  // checking solution came to realise _.orElse(None) is equivalent to (x => x), but I leave the mistake in here
  def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(_.orElse(None))
}

object Exercise47 {  
  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _) 
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E, List[A]] = traverse(es)(x => x)
}

object Exercise48 {
  //alternative implementation to accumulate errors
  sealed trait EitherList[+E, +A] 
  case class LeftL[+E](value: List[E]) extends EitherList[E, Nothing]
  case class RightL[+A](value: A) extends EitherList[Nothing, A]
}