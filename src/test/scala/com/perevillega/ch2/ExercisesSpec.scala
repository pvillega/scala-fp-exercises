package com.perevillega.ch2

import org.scalatest._

class ExercisesSpec extends FlatSpec with Matchers {
  "2.1" should "fib(0) is 0" in {
    Execise21.fib(0) should be(0)
  }
  it should "fib(1) is 1" in {
    Execise21.fib(1) should be(1)
  }
  it should "fib(2) is 1" in {
    Execise21.fib(2) should be(1)
  }
  it should "fib(3) is 2" in {
    Execise21.fib(3) should be(2)
  }
  it should "fib(4) is 3" in {
    Execise21.fib(4) should be(3)
  }
  it should "fib(30) is 832040" in {
    Execise21.fib(30) should be(832040)
  }
  it should "fib(100) is 354224848179261915075" in {
    Execise21.fib(100) should be(BigDecimal("354224848179261915075"))
  }

  "2.2" should "isSorted([]) is true" in {
    Exercise22.isSorted(Array(), (x:Int, y:Int) => x < y) should be(true)
  }
  it should "isSorted([1]) is true" in {
    Exercise22.isSorted(Array(1), (x:Int, y:Int) => x < y) should be(true)
  }
  it should "isSorted([1,2]) is true" in {
    Exercise22.isSorted(Array(1,2), (x:Int, y:Int) => x < y) should be(true)
  }
  it should "isSorted([2,1]) is false" in {
    Exercise22.isSorted(Array(2, 1), (x:Int, y:Int) => x < y) should be(false)
  }
  it should "isSorted([1,2,3,4,5,6]) is true" in {
    Exercise22.isSorted(Array(1,2,3,4,5,6), (x:Int, y:Int) => x < y) should be(true)
  }
  it should "isSorted([1,2,3,4,5,7,6]) is false" in {
    Exercise22.isSorted(Array(1,2,3,4,5,7,6), (x:Int, y:Int) => x < y) should be(false)
  }
  it should "isSorted(['a']) is true" in {
    Exercise22.isSorted(Array('a'), (x:Char, y:Char) => x < y) should be(true)
  }
  it should "isSorted(['a','b']) is true" in {
    Exercise22.isSorted(Array('a','b'), (x:Char, y:Char) => x < y) should be(true)
  }
  it should "isSorted(['b','a']) is false" in {
    Exercise22.isSorted(Array('b','a'), (x:Char, y:Char) => x < y) should be(false)
  }
  it should "isSorted(['a','b','c','d','e','f','g']) is true" in {
    Exercise22.isSorted(Array('a','b','c','d','e','f','g'), (x:Char, y:Char) => x < y) should be(true)
  }
  it should "isSorted(['a','b','c','d','e','f','h','g']) is false" in {
    Exercise22.isSorted(Array('a','b','c','d','e','f','h','g'), (x:Char, y:Char) => x < y) should be(false)
  }

  "2.3" should "curry works as expected" in {
  	val f: (Int, Int) => String = (x: Int, y : Int) => s"$x - $y"
    val curried = Exercise23.curry(f)
    val partial = curried(3)
    partial(4) should be("3 - 4")
  }

  "2.4" should "uncurry works as expected" in {
  	val f: Int => Int => String = (x: Int) => (y : Int) => s"$x - $y"
    val uncurried = Exercise24.uncurry(f)     
    uncurried(3,4) should be("3 - 4")
  }

  "2.5" should "compose works as expected" in {
  	val f: Int => String = (x: Int) => s"$x"
  	val g: Char => Int = (y: Char) => y.toInt
    val composed = Exercise25.compose(f,g)
    composed('A') should be("65")
  }
}
