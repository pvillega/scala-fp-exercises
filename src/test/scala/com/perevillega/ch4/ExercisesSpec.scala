package com.perevillega.ch4

import org.scalatest._

class ExercisesSpec extends FlatSpec with Matchers {
  "4.1" should "map of Some" in {
    Some(1).map(i => i + 1) should be(Some(2))
  }
  it should "flatMap of Some" in {
    Some(3).flatMap(i => Some(i + 1)) should be(Some(4))
  }
  it should "getOrElse of Some" in {
    Some(5).getOrElse(0) should be(5)
  }
  it should "orElse of Some" in {
    Some(6).orElse(Some(-1)) should be(Some(6))
  }
  it should "filter (true) of Some" in {
    Some(8).filter(i => i < 10) should be(Some(8))
  }
  it should "filter (false) of Some" in {
    Some(8).filter(i => i > 10) should be(None)
  }
  it should "map of None" in {
    None.map((i: Int) => i + 1) should be(None)
  }
  it should "flatMap of None" in {
    None.flatMap((i: Int) => Some(i + 1)) should be(None)
  }
  it should "getOrElse of None" in {
    None.getOrElse(1) should be(1)
  }
  it should "orElse of None" in {
    None.orElse(Some(4)) should be(Some(4))
  }
  it should "filter (true) of None" in {
    None.filter(i => true) should be(None)
  }
  it should "filter (false) of None" in {
    None.filter(i => false) should be(None)
  }

  "4.2" should "implement variance" in {
    val seq: Seq[Double] = List(1.0, 3.0, 5.0, 4.0, 2.0)
    Exercise42.variance(seq) should be(Some(2.0))
  }
  it should "return none if sequence is empty" in {
    val seq: Seq[Double] = Nil
    Exercise42.variance(seq) should be(None)
  }

  "4.3" should "map2 maps over option values" in {
    val a = Some(1)
    val b = Some(3)
    val f = (x: Int, y: Int) => x + y
    Exercise43.map2(a,b)(f) should be(Some(4))
  }
  it should "return none if first value is none" in {
    val a = None
    val b = Some(3)
    val f = (x: Int, y: Int) => x + y
    Exercise43.map2(a,b)(f) should be(None)
  }
  it should "return none if second value is none" in {
    val a = Some(1)
    val b = None
    val f = (x: Int, y: Int) => x + y
    Exercise43.map2(a,b)(f) should be(None)
  }

  "4.4" should "convert list of option into option of list" in {
    val seq: List[Option[Double]] = List(Some(1.0), Some(3.0), Some(5.0), Some(4.0), Some(2.0))
    val expected: Option[List[Double]] = Some(List(1.0, 3.0, 5.0, 4.0, 2.0))
    Exercise44.sequence(seq) should be(expected)
  }
  it should "return Some(Nil) if list is empty" in {
    val seq: List[Option[Double]] = Nil
    val expected: Option[List[Double]] = Some(Nil)
    Exercise44.sequence(seq) should be(expected)
  }
  it should "return None if list contains None" in {
    val seq: List[Option[Double]] = List(Some(1.0), Some(3.0), Some(5.0), None, Some(2.0))
    val expected: Option[List[Double]] = None
    Exercise44.sequence(seq) should be(expected)
  }

  "4.5" should "traverse transfroms a list into an option of a list" in {
    val seq = List("1","2","3")
    val expected = Some(List(1, 2, 3))
    def f = (x: String) => Some(x.toInt)
    Exercise45.traverse(seq)(f) should be(expected)
  }
   it should "traverse returns Some(Nil) if list is empty" in {
    val seq = Nil
    val expected = Some(Nil)
    def f = (x: String) => Some(x.toInt)
    Exercise45.traverse(seq)(f) should be(expected)
  }
  it should "traverse returns None if list contains a non-aplicable transformation" in {
    val seq = List("1","A","3")
    val expected = None
    def f = (x: String) => try Some(x.toInt) catch { case e: Exception => None }
    Exercise45.traverse(seq)(f) should be(expected)
  }
  it should "sequence converts list of option into option of list" in {
    val seq: List[Option[Double]] = List(Some(1.0), Some(3.0), Some(5.0), Some(4.0), Some(2.0))
    val expected: Option[List[Double]] = Some(List(1.0, 3.0, 5.0, 4.0, 2.0))
    Exercise45.sequence(seq) should be(expected)
  }
  it should "sequence returns Some(Nil) if list is empty" in {
    val seq: List[Option[Double]] = Nil
    val expected: Option[List[Double]] = Some(Nil)
    Exercise45.sequence(seq) should be(expected)
  }
  it should "sequence returns None if list contains None" in {
    val seq: List[Option[Double]] = List(Some(1.0), Some(3.0), Some(5.0), None, Some(2.0))
    val expected: Option[List[Double]] = None
    Exercise45.sequence(seq) should be(expected)
  }

  "4.6" should "map of Left" in {
    val value = Left(new Exception())
    value.map((x: String) => x.toInt) should be(value)
  }
  it should "map of Right" in {
    Right("4").map(_.toInt) should be(Right(4))
  }
  it should "flatMap of Left" in {
    val value = Left(new Exception())
    value.flatMap((x: String) => Right(x.toInt)) should be(value) 
  }
  it should "flatMap of Right" in {
    Right("4").flatMap(x => Right(x.toInt)) should be(Right(4))
  }
  it should "orElse of Left" in {
    Left(new Exception()).orElse(Right(4)) should be(Right(4))
  }
  it should "orElse of Right" in {
    Right("A").orElse(Right(4)) should be(Right("A"))
  }
  it should "map2 of Left" in {
    val value = Left(new Exception())
    value.map2(Right("4"))((x: String, y: String) => x.toInt + y.toInt) should be(value)
  }
  it should "map2 of Right" in {
    Right("8").map2(Right("4"))((x: String, y: String) => x.toInt + y.toInt) should be(Right(12))
  }

  "4.7" should "traverse transfroms a list into an either of a list" in {
    val seq = List("1","2","3")
    val expected = Right(List(1, 2, 3))
    def f = (x: String) => Right(x.toInt)
    Exercise47.traverse(seq)(f) should be(expected)
  }
   it should "traverse returns Some(Nil) if list is empty" in {
    val seq = Nil
    val expected = Right(Nil)
    def f = (x: String) => Right(x.toInt)
    Exercise47.traverse(seq)(f) should be(expected)
  }
  it should "traverse returns Left if list contains a non-aplicable transformation" in {
    val seq = List("1","A","3")
    val expected = Left(new NumberFormatException())
    def f = (x: String) => try Right(x.toInt) catch {case e: Exception => expected }
    Exercise47.traverse(seq)(f) should be(expected)
  }
  it should "sequence converts list of Either into Either of list" in {
    val seq = List(Right(1.0), Right(3.0), Right(5.0), Right(4.0), Right(2.0))
    val expected = Right(List(1.0, 3.0, 5.0, 4.0, 2.0))
    Exercise47.sequence(seq) should be(expected)
  }
  it should "sequence returns Some(Nil) if list is empty" in {
    val seq: List[Either[Exception, Double]] = Nil
    val expected = Right(Nil)
    Exercise47.sequence(seq) should be(expected)
  }
  it should "sequence returns Left if list contains Left" in {
    val expected = Left(new Exception)
    val seq = List(Right(1.0), Right(3.0), Right(5.0), expected, Right(2.0))    
    Exercise47.sequence(seq) should be(expected)
  }
}