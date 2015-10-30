package com.perevillega.ch9

import com.perevillega.ch8._
import com.perevillega.ch8.Prop._

import scala.language.implicitConversions

trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))((a, b) => a :: b) | succeed(List())

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = ???

  def slice[A](p: Parser[A]): Parser[String] = ???

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n < 1) succeed(List())
    else map2(p, listOfN(n - 1, p))((a, b) => a :: b)

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    map(product(p, p2))(t => f(t._1, t._2))

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def **[B](p2: Parser[B]): Parser[(A, B)] = product(p, p2)

  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: SGen[String]): Prop =
      Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: SGen[String]): Prop =
      equal(p, p.map(a => a))(in)
  }

}

