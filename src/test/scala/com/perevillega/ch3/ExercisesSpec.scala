package com.perevillega.ch3

import org.scalatest._

class ExercisesSpec extends FlatSpec with Matchers {
  "3.1" should "match result is 3" in {
    Exercise31.matched should be(3)
  }

  "3.2" should "tail removes the first element of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise32.tail(list) should be(Cons(4, Cons(5, Nil)))
  }
  it should "return nil if tail of empty list" in {
    Exercise32.tail(Nil) should be(Nil)	
  }
 
  "3.3" should "setHead replaces the first element of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise33.setHead(list, 9) should be(Cons(9, Cons(4, Cons(5, Nil))))
  }
  it should "raise exception" in {
  	intercept[IllegalArgumentException] {
      Exercise33.setHead(Nil, 9)	
	}
  }

  "3.4" should "drop removes the first 0 elements of a list returns original list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise34.drop(list, 0) should be(list)
  }
  it should "drop removes the first 1 elements of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise34.drop(list, 1) should be(Cons(4, Cons(5, Nil)))
  }
  it should "drop removes the first 2 elements of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise34.drop(list, 2) should be(Cons(5, Nil))
  }
  it should "return nil if drop of empty list" in {
    Exercise34.drop(Nil, 2) should be(Nil)	
  }

  "3.5" should "dropWhile removes 0 elements of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise35.dropWhile(list, (x: Int) => false) should be(list)
  }
  it should "dropWhile removes the first 1 elements of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise35.dropWhile(list, (x: Int) => x < 4) should be(Cons(4, Cons(5, Nil)))
  }
  it should "dropWhile removes the first 2 elements of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise35.dropWhile(list, (x: Int) => x < 5) should be(Cons(5, Nil))
  }
  it should "return nil if dropWhile of empty list" in {
    Exercise35.dropWhile(Nil, (x: Int) => true) should be(Nil)	
  }

  "3.6" should "init removes last element of a list" in {
  	val list = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise36.init(list) should be(Cons(3, Cons(4, Nil)))
  }
  it should "return nil if init of empty list" in {
    Exercise36.init(Nil) should be(Nil)	
  }

  "3.7" should "product using foldright" in {
    val list: List[Double] = Cons(3, Cons(4, Cons(5, Nil)))
    Exercise37.product(list) should be(60)
  }

  "3.8" should "passing Cons to foldRight should create the same list" in {    
    val list = Cons(1, Cons(2, Cons(3, Nil)))
    Exercise38.test should be(list)
  }

  "3.9" should "length of empty list is 0" in {    
    val list = Nil
    Exercise39.length(list) should be(0)
  }
  it should "length of list is 1" in {    
    val list = Cons(1, Nil)
    Exercise39.length(list) should be(1)
  }
  it should "length of list is 10" in {    
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(0, Nil))))))))))
    Exercise39.length(list) should be(10)
  }

  "3.10" should "foldRight is not stack-safe" in {        
    val list = Exercise310.massiveList
    intercept[StackOverflowError]{    
      List.foldRight(list, 1.0)(_ * _)
    }
  }
  it should "work fine with tailrec foldLeft" in {        
    val list = Exercise310.massiveList
    Exercise310.foldLeft(list, 0.0)(_ * _) should be(0)
  } 


  "3.11" should "length of empty list is 0" in {    
    val list = Nil
    Exercise311.length(list) should be(0)
  }
  it should "length of list is 1" in {    
    val list = Cons(1, Nil)
    Exercise311.length(list) should be(1)
  }
  it should "length of list is 10" in {    
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(0, Nil))))))))))
    Exercise311.length(list) should be(10)
  }
  it should "sum of empty list is 0" in {    
    val list = Nil
    Exercise311.sum(list) should be(0)
  }
  it should "sum of list is 1" in {    
    val list = Cons(1, Nil)
    Exercise311.sum(list) should be(1)
  }
  it should "sum of list is 45" in {    
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(0, Nil))))))))))
    Exercise311.sum(list) should be(45)
  }
  it should "product of empty list is 1" in {    
    val list = Nil
    Exercise311.product(list) should be(1)
  }
  it should "product of list is 2" in {    
    val list: List[Double] = Cons(2, Nil)
    Exercise311.product(list) should be(2)
  }
  it should "product of list is 0" in {    
    val list: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Cons(0, Nil))))))))))
    Exercise311.product(list) should be(0)
  }

  "3.12" should "reverse a list" in {    
    val list = Cons(3, Cons(4, Cons(6, Nil)))
    val reversed = Cons(6, Cons(4, Cons(3, Nil)))
    Exercise312.reverse(list) should be(reversed)
  }
  it should "reverse an empty list is an empty list" in {    
    val list = Nil
    Exercise312.reverse(list) should be(Nil)
  }

  "3.13" should "foldLeft based on foldRight" in {        
    val list = Cons(6, Cons(4, Cons(3, Nil)))
    Exercise313.foldLeft(list, 1.0)(_ * _) should be(72)
  } 
  it should "foldRight based on foldLeft" in {    
    //we reuse massive list to prove we are tail rec now
    val list = Exercise310.massiveList
    Exercise313.foldRight(list, 0)(_ * _) should be(0)
  }

  "3.14" should "append based on foldRight" in {        
    val list = Cons(6, Cons(4, Cons(3, Nil)))
    val expected = Cons(6, Cons(4, Cons(3, Cons(5, Nil))))
    Exercise314.append(list, 5) should be(expected)
  } 

  "3.15" should "flatten on empty list returns Nil" in {        
    val list = Nil
    val expected = Nil
    Exercise315.flatten(list) should be(expected)
  }
  it should "flatten a list with 1 element" in {        
    val list = Cons(Cons(2,Cons(3, Nil)), Nil)
    val expected = Cons(2,Cons(3, Nil))
    Exercise315.flatten(list) should be(expected)
  }
  it should "flatten a list with several elements" in {        
    val list = Cons(Cons(2,Cons(3, Nil)), Cons(Cons(4,Cons(5, Nil)), Cons(Cons(6,Cons(7, Nil)), Nil)))
    val expected = Cons(2,Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Nil))))))
    Exercise315.flatten(list) should be(expected)
  }

  "3.16" should "increment on empty list returns Nil" in {        
    val list = Nil
    val expected = Nil
    Exercise316.increment(list) should be(expected)
  }
  it should "increment a list with 1 element" in {        
    val list = Cons(2, Nil)
    val expected = Cons(3, Nil)
    Exercise316.increment(list) should be(expected)
  }
  it should "increment a list with several elements" in {        
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val expected = Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Nil))))))
    Exercise316.increment(list) should be(expected)
  }

  "3.17" should "doubleToString on empty list returns Nil" in {        
    val list = Nil
    val expected = Nil
    Exercise317.doubleToString(list) should be(expected)
  }
  it should "doubleToString a list with 1 element" in {        
    val list: List[Double] = Cons(2, Nil)
    val expected = Cons("2.0", Nil)
    Exercise317.doubleToString(list) should be(expected)
  }
  it should "doubleToString a list with several elements" in {        
    val list: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val expected = Cons("1.0", Cons("2.0", Cons("3.0", Cons("4.0", Cons("5.0", Cons("6.0", Nil))))))
    Exercise317.doubleToString(list) should be(expected)
  }

  "3.18" should "map on empty list returns Nil" in {        
    val list = Nil
    val expected = Nil
    Exercise318.map(list)(_.toString) should be(expected)
  }
  it should "map a list with 1 element" in {        
    val list: List[Double] = Cons(2, Nil)
    val expected = Cons("2.0", Nil)
    Exercise318.map(list)(_.toString) should be(expected)
  }
  it should "map a list with several elements" in {        
    val list: List[Double] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val expected = Cons("1.0", Cons("2.0", Cons("3.0", Cons("4.0", Cons("5.0", Cons("6.0", Nil))))))
    Exercise318.map(list)(_.toString) should be(expected)
  }

  "3.19" should "filter on empty list returns Nil" in {        
    val list: List[Int] = Nil
    val expected = Nil
    Exercise319.filter(list)(_ % 2 == 0) should be(expected)
  }
  it should "filter odd elements out of a list" in {        
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val expected = Cons(2, Cons(4, Cons(6, Nil)))
    Exercise319.filter(list)(_ % 2 == 0) should be(expected)
  }

  "3.20" should "flatMap on empty list returns Nil" in {        
    val list: List[Int] = Nil
    val expected = Nil
    Exercise320.flatMap(list)(i => List(i,i)) should be(expected)
  }
  it should "flatMap a list" in {        
    val list = List(1,2,3)
    val expected = List(1,1,2,2,3,3)
    Exercise320.flatMap(list)(i => List(i,i)) should be(expected)
  }

  "3.21" should "filter on empty list returns Nil" in {        
    val list: List[Int] = Nil
    val expected = Nil
    Exercise321.filter(list)(_ % 2 == 0) should be(expected)
  }
  it should "filter odd elements out of a list" in {        
    val list = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
    val expected = Cons(2, Cons(4, Cons(6, Nil)))
    Exercise321.filter(list)(_ % 2 == 0) should be(expected)
  }

  "3.22" should "sum on empty lists returns Nil" in {        
    val list: List[Int] = Nil
    val list2: List[Int] = Nil
    val expected = Nil
    Exercise322.sum(list, list2) should be(expected)
  }
  it should "sum with one empty list returns Nil" in {        
    val list: List[Int] = Nil
    val list2: List[Int] = List(1)
    val expected = Nil
    Exercise322.sum(list, list2) should be(expected)
  }
  it should "sum two lists" in {        
    val list = List(1,2,3)
    val list2 = List(4,5,6)
    val expected = List(5,7,9)
    Exercise322.sum(list, list2) should be(expected)
  }

  "3.23" should "zipWith on empty lists returns Nil" in {        
    val list: List[Int] = Nil
    val list2: List[Int] = Nil
    val expected = Nil
    Exercise323.zipWith(list, list2)(_ + _) should be(expected)
  }
  it should "zipWith with one empty list returns Nil" in {        
    val list: List[Int] = Nil
    val list2: List[Int] = List(1)
    val expected = Nil
    Exercise323.zipWith(list, list2)(_ + _) should be(expected)
  }
  it should "zipWith two lists" in {        
    val list = List(1,2,3)
    val list2 = List(4,5,6)
    val expected = List(5,7,9)
    Exercise323.zipWith(list, list2)(_ + _) should be(expected)
  }

  "3.24" should "hasSubsequence on empty lists returns true" in {        
    val list: List[Int] = Nil
    val list2: List[Int] = Nil
    Exercise324.hasSubsequence(list, list2) should be(true)
  }
  it should "hasSubsequence of Nil on list returns true" in {        
    val list: List[Int] = List(1,2,3,4)
    val list2: List[Int] = Nil
    Exercise324.hasSubsequence(list, list2) should be(true)
  }
  it should "hasSubsequence on list returns true if exists" in {        
    val list: List[Int] = List(1,2,3,4,5,6,7,8,9)
    val list2: List[Int] = List(3,4)
    Exercise324.hasSubsequence(list, list2) should be(true)
  }
  it should "hasSubsequence on list returns false if doesn't exists" in {        
    val list: List[Int] = List(1,2,3,4,5,6,7,8,9)
    val list2: List[Int] = List(3,6)
    Exercise324.hasSubsequence(list, list2) should be(false)
  }

  "3.25" should "size of leaf tree returns 1" in {        
    val tree = Leaf(2)
    Exercise325.size(tree) should be(1)
  }
  it should "return size of the tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise325.size(tree) should be(13)
  }

  "3.26" should "max of leaf tree returns value" in {        
    val tree = Leaf(2)
    Exercise326.max(tree) should be(2)
  }
  it should "return max of the tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise326.max(tree) should be(9)
  }

  "3.27" should "depth of leaf tree is 1" in {        
    val tree = Leaf(2)
    Exercise327.depth(tree) should be(1)
  }
  it should "return depth of the tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise327.depth(tree) should be(5)
  }

  "3.28" should "map of leaf tree" in {        
    val tree = Leaf(2)
    val expected = Leaf(4)
    Exercise328.map(tree)(_ * 2) should be(expected)
  }
  it should "map of tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    val expected = Branch(Branch(Branch(Leaf(6), Leaf(18)), Leaf(8)), Branch(Leaf(12), Branch(Leaf(0), Branch(Leaf(8), Leaf(16)))))
    Exercise328.map(tree)(_ * 2) should be(expected)
  }

  "3.29" should "fold of leaf tree" in {        
    val tree = Leaf(2)
    Exercise329.fold(tree)(_ * 2)(_ max _) should be(4)
  }
  it should "fold of tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise329.fold(tree)(_ * 2)(_ max _) should be(18)
  }
  it should "size of leaf tree returns 1" in {        
    val tree = Leaf(2)
    Exercise329.size(tree) should be(1)
  }
  it should "return size of the tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise329.size(tree) should be(13)
  }
  it should "max of leaf tree returns value" in {        
    val tree = Leaf(2)
    Exercise329.max(tree) should be(2)
  }
  it should "return max of the tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise329.max(tree) should be(9)
  }
  it should "depth of leaf tree is 1" in {        
    val tree = Leaf(2)
    Exercise329.depth(tree) should be(1)
  }
  it should "return depth of the tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    Exercise329.depth(tree) should be(5)
  }
  it should "map of leaf tree" in {        
    val tree = Leaf(2)
    val expected = Leaf(4)
    Exercise329.map(tree)(_ * 2) should be(expected)
  }
  it should "map of tree" in {        
    val tree = Branch(Branch(Branch(Leaf(3), Leaf(9)), Leaf(4)), Branch(Leaf(6), Branch(Leaf(0), Branch(Leaf(4), Leaf(8)))))
    val expected = Branch(Branch(Branch(Leaf(6), Leaf(18)), Leaf(8)), Branch(Leaf(12), Branch(Leaf(0), Branch(Leaf(8), Leaf(16)))))
    Exercise329.map(tree)(_ * 2) should be(expected)
  }
}
