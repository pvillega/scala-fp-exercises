package com.perevillega.ch5

import org.scalatest._

class ExercisesSpec extends FlatSpec with Matchers {
  "5.1" should "toList of empty stream" in {
  	val stream = Stream.empty
  	val expected = Nil
    stream.toList should be(expected)
  }
  it should "toList of stream with one element" in {
  	val stream = Stream(1)
  	val expected = List(1)
  	stream.toList should be(expected)
  }
  it should "toList of stream with several element" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = List(1,2,3,4,5,6,7,8,9,0)
  	stream.toList should be(expected)
  }

  "5.2" should "take(1) of empty stream" in {  	
  	val stream = Stream.empty
  	val expected = Stream.empty
    stream.take(1) should be(expected)
  }
  it should "take(0) of stream" in {
  	val stream = Stream(1)
  	val expected = Stream.empty
  	stream.take(0) should be(expected)
  }
  it should "take(1) of Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(1)
  	stream.take(1).toList should be(expected.toList)
  }
  it should "take(n) of Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(1,2,3,4,5)
  	stream.take(5).toList should be(expected.toList)
  }
  it should "if n bigger than stream length take(n) returns full Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(1,2,3,4,5,6,7,8,9,0)
  	stream.take(100).toList should be(expected.toList)
  }
  it should "drop(1) of empty stream" in {  	
  	val stream = Stream.empty
  	val expected = Stream.empty
    stream.drop(1) should be(expected)
  }
  it should "drop(0) of stream" in {
  	val stream = Stream(1)
  	val expected = Stream(1)
  	stream.drop(0).toList should be(expected.toList)
  }
  it should "drop(1) of Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(2,3,4,5,6,7,8,9,0)
  	stream.drop(1).toList should be(expected.toList)
  }
  it should "drop(n) of Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(6,7,8,9,0)
  	stream.drop(5).toList should be(expected.toList)
  }
  it should "if n bigger than stream length drop(n) returns empty Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream.empty
  	stream.drop(100).toList should be(expected.toList)
  }

  "5.3" should "takeWhile(true) of empty stream" in {  	
  	val stream = Stream.empty
  	val expected = Stream.empty
    stream.takeWhile(x => x == x) should be(expected)
  }
  it should "takeWhile(false) of empty stream" in {
  	val stream = Stream.empty
  	val expected = Stream.empty
  	stream.takeWhile(x => x != x) should be(expected)
  }
  it should "takeWhile(false) of Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream.empty
  	stream.takeWhile(x => x != x).toList should be(expected.toList)
  }
  it should "takeWhile(true) of full Stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(1,2,3,4,5,6,7,8,9,0)
  	stream.takeWhile(x => x == x).toList should be(expected.toList)
  }
  it should "takeWhile only conforms fraction fo stream" in {
  	val stream = Stream(1,2,3,4,5,6,7,8,9,0)
  	val expected = Stream(1,2,3,4,5)
  	stream.takeWhile(_ < 6).toList should be(expected.toList)
  }

  "5.4" should "forAll of empty stream" in {
    val stream = Stream.empty
    val expected = true
    val p = (x: Int) => x < 8
    stream.forAll(p) should be(expected)
  }
  it should "forAll of stream with one element which complies" in {
    val stream = Stream(1)
    val expected = true
    val p = (x: Int) => x < 8
    stream.forAll(p) should be(expected)
  }
  it should "forAll of stream with one element which doesn't complie" in {
    val stream = Stream(1)
    val expected = false
    val p = (x: Int) => x > 8
    stream.forAll(p) should be(expected)
  }
  it should "forAll of stream with several elements all valid" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = true
    val p = (x: Int) => x > -1
    stream.forAll(p) should be(expected)
  }
  it should "forAll of stream with several elements some invalid" in {
    val stream = Stream(1,2,3,4,5,9,8,6,7)
    val expected = false
    val p = (x: Int) => x < 8
    stream.forAll(p) should be(expected)
  }

  "5.5" should "takeWhile(true) of empty stream" in {   
    val stream = Stream.empty
    val expected = Stream.empty
    stream.takeWhileFR(x => x == x) should be(expected)
  }
  it should "takeWhile(false) of empty stream" in {
    val stream = Stream.empty
    val expected = Stream.empty
    stream.takeWhileFR(x => x != x) should be(expected)
  }
  it should "takeWhile(false) of Stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream.empty
    stream.takeWhileFR(x => x != x).toList should be(expected.toList)
  }
  it should "takeWhile(true) of full Stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream(1,2,3,4,5,6,7,8,9,0)
    stream.takeWhileFR(x => x == x).toList should be(expected.toList)
  }
  it should "takeWhile only conforms fraction fo stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream(1,2,3,4,5)
    stream.takeWhileFR(_ < 6).toList should be(expected.toList)
  }

  "5.6" should "headOption of empty stream" in {   
    val stream = Stream.empty
    val expected = None
    stream.headOptionFR should be(expected)
  }
  it should "headOption of stream" in {
    val stream = Stream(1)
    val expected = Some(1)
    stream.headOptionFR should be(expected)
  }

  "5.7" should "append on empty stream" in {        
    val stream = Stream.empty[Int]    
    val expected = Stream(1)
    stream.append(Stream(1)).toList should be(expected.toList)
  } 
  it should "append on stream" in {        
    val stream = Stream(1,2,3,4,5)
    val expected = Stream(1,2,3,4,5,9)
    stream.append(Stream(9)).toList should be(expected.toList)
  }
  it should "append empty on stream" in {        
    val stream = Stream(1,2,3,4,5)
    val expected = Stream(1,2,3,4,5)
    stream.append(Stream.empty[Int]).toList should be(expected.toList)
  } 
  it should "map on empty stream" in {        
    val stream = Stream.empty[Int]    
    val expected = Stream.empty
    stream.map(x => 2 * x).toList should be(expected.toList)
  }
  it should "map on stream" in {        
    val stream = Stream(1,2,3,4)
    val expected = Stream(2,4,6,8)
    stream.map(x => 2 * x).toList should be(expected.toList)
  } 
  it should "filter on empty stream" in {        
    val stream = Stream.empty[Int]    
    val expected = Stream.empty
    stream.filter(x => x > 2).toList should be(expected.toList)
  }
  it should "filter on stream" in {        
    val stream = Stream(1,2,3,4)
    val expected = Stream(3,4)
    stream.filter(x => x > 2).toList should be(expected.toList)
  } 
  it should "filter out a full stream" in {        
    val stream = Stream(1,2,3,4)
    val expected = Stream.empty
    stream.filter(x => x > 6).toList should be(expected.toList)
  } 
  it should "flatMap on empty stream" in {        
    val stream = Stream.empty[Int]    
    val expected = Stream.empty
    stream.flatMap(x => Stream(x.toString)).toList should be(expected.toList)
  }
  it should "flatMap on stream" in {        
    val stream = Stream(1,2,3,4)
    val expected = Stream("1","2","3","4")
    stream.flatMap(x => Stream(x.toString)).toList should be(expected.toList)
  }

  "5.8" should "constant provides an infinite stream" in {
    val stream = Stream.constant(1)
    stream.forAll(_ != 1) should be(right = false)
  }

  "5.9" should "generate ordered stream of integers" in {
    val stream = Stream.from(1)
    stream.take(5).toList should be(List(1,2,3,4,5))
  }
  it should "generate infinite stream of integers" in {
    val stream = Stream.from(1)
    stream.filter(_ >= 100).headOption should be(Some(100))
  }

  "5.10" should "generate fibonacci numbers" in {
    val stream = Stream.fibs()
    stream.take(7).toList should be(List(0,1,1,2,3,5,8))
  }

  "5.11" should "unfold can build empty stream" in {
    val stream = Stream.unfold(0)(_ => None)
    stream.toList should be(Nil)
  }
  it should "unfold can build a limited stream of chars" in {
    val stream = Stream.unfold(0)(f => if(f < 5) Some((('a' + f).toChar.toString, f + 1)) else  None)
    stream.toList should be(List("a","b","c","d","e"))
  }
  it should "unfold can build an infinite stream of natural integers" in {
    val stream = Stream.unfold(0)(f => Some(f, f+1))
    stream.take(10).toList should be(List(0,1,2,3,4,5,6,7,8,9))
    stream.drop(100000).headOption should be(Some(100000))
  }
  it should "unfold can build fibonacci streams" in {
    val stream = Stream.unfold((0,1))(f => Some(f._1, (f._2, f._1 + f._2)))
    stream.take(7).toList should be(List(0,1,1,2,3,5,8))
  }

  "5.12" should "fibs using unfold" in {
    val stream = Stream.fibsUnfold()
    stream.take(7).toList should be(List(0,1,1,2,3,5,8))
  }
  it should "from using unfold" in {
    val stream = Stream.fromUnfold(1)
    stream.filter(_ >= 100).headOption should be(Some(100))
  }
  it should "constant/ones using unfold" in {
    // ones is a particular case from constant so covered
    val stream = Stream.constantUnfold(1)
    stream.forAll(_ != 1) should be(right = false)
  }

  "5.13" should "map on empty stream" in {
    val stream = Stream.empty[Int]
    val expected = Stream.empty
    stream.mapUnfold(x => 2 * x).toList should be(expected.toList)
  }
  it should "map on stream" in {
    val stream = Stream(1,2,3,4)
    val expected = Stream(2,4,6,8)
    stream.mapUnfold(x => 2 * x).toList should be(expected.toList)
  }
  it should "take(1) of empty stream" in {
    val stream = Stream.empty
    val expected = Stream.empty
    stream.takeUnfold(1) should be(expected)
  }
  it should "take(0) of stream" in {
    val stream = Stream(1)
    val expected = Stream.empty
    stream.takeUnfold(0) should be(expected)
  }
  it should "take(1) of Stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream(1)
    stream.takeUnfold(1).toList should be(expected.toList)
  }
  it should "take(n) of Stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream(1,2,3,4,5)
    stream.takeUnfold(5).toList should be(expected.toList)
  }
  it should "takeWhile(true) of empty stream" in {
    val stream = Stream.empty
    val expected = Stream.empty
    stream.takeWhileUnfold(x => x == x) should be(expected)
  }
  it should "takeWhile(false) of empty stream" in {
    val stream = Stream.empty
    val expected = Stream.empty
    stream.takeWhileUnfold(x => x != x) should be(expected)
  }
  it should "takeWhile(false) of Stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream.empty
    stream.takeWhileUnfold(x => x != x).toList should be(expected.toList)
  }
  it should "takeWhile(true) of full Stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream(1,2,3,4,5,6,7,8,9,0)
    stream.takeWhileUnfold(x => x == x).toList should be(expected.toList)
  }
  it should "takeWhile only conforms fraction fo stream" in {
    val stream = Stream(1,2,3,4,5,6,7,8,9,0)
    val expected = Stream(1,2,3,4,5)
    stream.takeWhileUnfold(_ < 6).toList should be(expected.toList)
  }
  it should "zipWith on empty Streams returns empty Stream" in {
    val s1 = Stream.empty[Int]
    val s2 = Stream.empty[Int]
    val expected = Stream.empty[Int]
    s1.zipWith(s2)(_ + _) should be(expected)
  }
  it should "zipWith with one empty stream returns empty Stream" in {
    val s1 = Stream.empty[Int]
    val s2 = Stream.from(1)
    val expected = Stream.empty[Int]
    s1.zipWith(s2)(_ + _) should be(expected)
  }
  it should "zipWith two streams" in {
    val s1 = Stream.from(0)
    val s2 = Stream.from(1)
    val expected = Stream.unfold((0,1))(f => Some(f._1 + f._2, (f._1 + 1, f._2 + 1)))
    s1.zipWith(s2)(_ + _).take(10).toList should be(expected.take(10).toList)
  }
  it should "zipAll on empty Streams" in {
    val s1 = Stream.empty[Int]
    val s2 = Stream.empty[Int]
    val expected = Stream.empty[Int]
    s1.zipAll(s2) should be(expected)
  }
  it should "zipAll with one empty stream" in {
    val s1 = Stream.empty[Int]
    val s2 = Stream.from(1)
    val expected = Stream.unfold(1)(f => Some((None, Some(f)), f + 1))
    s1.zipAll(s2).take(10).toList should be(expected.take(10).toList)
  }
  it should "zipAll two infinite streams" in {
    val s1 = Stream.from(0)
    val s2 = Stream.from(1)
    val expected = Stream.unfold((0,1))(f => Some((Some(f._1), Some(f._2)), (f._1 + 1, f._2 + 1)))
    s1.zipAll(s2).take(10).toList should be(expected.take(10).toList)
  }

  "5.14" should "startsWith empty stream is true for an empty stream" in {
    val s1 = Stream.empty[Int]
    val s2 = Stream.empty[Int]
    s1.startsWith(s2) should be(true)
  }
  it should "startsWith empty stream is true for a non-empty stream" in {
    val s1 = Stream.from(1)
    val s2 = Stream.empty[Int]
    s1.startsWith(s2) should be(true)
  }
  it should "startsWith a valid stream returns true" in {
    val s1 = Stream.from(1)
    val s2 = Stream(1,2,3,4)
    s1.startsWith(s2) should be(true)
  }
  it should "startsWith an invalid stream returns false" in {
    val s1 = Stream.from(1)
    val s2 = Stream(5,6,7,8)
    s1.startsWith(s2) should be(false)
  }

  "5.15" should "tails of empty stream is empty stream" in {
    val s1 = Stream.empty[Int]
    val expected = Stream(Stream.empty[Int])
    s1.tails.map(_.toList).toList should be(expected.map(_.toList).toList)
  }
  it should "tails for non-infinite stream" in {
    val s1 = Stream(1,2,3)
    val expectedStream = Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream.empty[Int])
    val expected = expectedStream.map(_.toList).toList
    s1.tails.map(_.toListTailRec).toListTailRec should be(expected)
  }

  // 5.16 to be done later
}