package com.perevillega.ch2

object Execise21 {
  def fib(n: Int): BigDecimal = {
 	// to be tailrec we have to work 'incrementally' by calculating the number from the base ones (0, 1)
 	// instead of recursively calling fib(n - 1) + fib(n - 2). It makes sense as if we want tailrec
 	// we want something that will collapse to a loop, so the behaviour will be similar 
 	@annotation.tailrec
 	def fibRec(step: Int, p: BigDecimal, q: BigDecimal): BigDecimal =
 		if(step == 0) p  		
 		else fibRec(step - 1, q, p + q)

 	fibRec(n, 0, 1)
  }
}

object Exercise22 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  	@annotation.tailrec
  	def verifySort(as: Array[A], ordered: (A,A) => Boolean): Boolean = 
  		if(as.length < 2) true
  		else if(!ordered(as(0), as(1))) false
  		else verifySort(as.tail, ordered)

  	verifySort(as, ordered)
  }

}

object Exercise23 {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = (x: A) => (y: B) => f(x,y)
}

object Exercise24 {
  def uncurry[A,B,C](f: A => B => C): (A,B) => C = (x: A, y: B) => f(x)(y)
}

object Exercise25 {
  def compose[A,B,C](f: B => C, g: A => B): A => C = (x: A) => f(g(x))
}