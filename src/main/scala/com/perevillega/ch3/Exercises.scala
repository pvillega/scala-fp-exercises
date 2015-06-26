package com.perevillega.ch3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
  	case Nil => 0
  	case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
  	case Nil => 1.0
  	case Cons(0.0, _) => 0.0
  	case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = 
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = 
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }   
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Exercise31 {
  def matched: Int = List(1,2,3,4,5) match {
  	case Cons(x, Cons(2, Cons(4, _))) => x
  	case Nil => 42
  	case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
  	case Cons(h, t) => h + List.sum(t)
  	case _ => 101
  }
}

object Exercise32 {
  //on empty list I could raise exception or just return Nil, 
  //I choose to return Nil to avoid breaking the flow during runtime, assuming all other methods will behave
  //'nicely' on empty lists and provide default values
  def tail[A](l: List[A]):List[A] = l match {
    case Nil => Nil
    case Cons(h, t) => t
  }
}

object Exercise33 {
  def setHead[A](l: List[A], head: A):List[A] = l match {
    case Nil => throw new IllegalArgumentException("Can't setHead on empty list")
    case Cons(h, t) => Cons(head, t)
  }
}

object Exercise34 {
  //on empty list I behave as in exercise 3.2  
  def drop[A](l: List[A], n: Int):List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if n > 0 => drop(t, n-1)
    case _ => l
  }
}

object Exercise35 {
  //on empty list I behave as in exercise 3.2  
  def dropWhile[A](l: List[A], f: A => Boolean):List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }
}

object Exercise36 {
  //on empty list I behave as in exercise 3.2  
  def init[A](l: List[A]):List[A] = {
    def initRec[A](origin: List[A]): List[A] = origin match {
      case Nil => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, initRec(t))
    }

    initRec(l)
  }
}

object Exercise37 {
  def product(ds: List[Double]): Double = {
    // we can't shortcircuit, if we see foldRight we call the recursive parameter even before evaluating the method
    // another implementation might help (Option? Either?) not sure how though    
    List.foldRight(ds, 1.0)(_ * _)
  }
}

object Exercise38 {
  def test = List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_,_))
}

object Exercise39 {
  def length[A](as: List[A]): Int = List.foldRight(as, 0)((x: A, y: Int) => y + 1)
}

object Exercise310 {
  lazy val massiveList: List[Int] = {
    // we can't generate a list of 10k elements like this due to stack overflow on List.apply(), so we create a couple and merge them
    val list = List.apply((1 to 4000).toList: _*)        
    val list2 = List.apply((1 to 4000).toList: _*)        
    
    def merge[A](source: List[A], append: List[A]): List[A] = source match {
      case Nil => Nil // we should never get here but to make compiler happy
      case Cons(h, Nil) => Cons(h, append)
      case Cons(h, t) => Cons(h, merge(t, append))
    }

    merge(list, list2)
  } 

  def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    @annotation.tailrec
    def tailRecFold(l: List[A], acc: B): B = l match {
      case Nil => acc
      case Cons(h, t) => tailRecFold(t, f(acc, h))
    } 

    tailRecFold(as, z)

  }
}

object Exercise311 {
  def sum(ints: List[Int]): Int = Exercise310.foldLeft(ints, 0)(_ + _)
  def product(ds: List[Double]): Double = Exercise310.foldLeft(ds, 1.0)(_ * _)
  def length[A](as: List[A]): Int = Exercise310.foldLeft(as, 0)((x,y) => x + 1)
}

object Exercise312 {
  def reverse[A](as: List[A]): List[A] = Exercise310.foldLeft(as, Nil: List[A])((x, y) => Cons(y,x))
}

object Exercise313 {
   def foldLeft[A,B](as: List[A], z: B)(f: (B,A) => B): B = List.foldRight(as, z)((a,b) => f(b,a))
   def foldRight[A,B](as: List[A], z:B)(f: (A,B) => B): B = Exercise310.foldLeft(as, z)((b,a) => f(a,b))
}

object Exercise314 {
  def append[A](as: List[A], h: A): List[A] = List.foldRight(as, Cons(h, Nil))(Cons(_,_))
}

object Exercise315 {
  def flatten[A](as: List[List[A]]): List[A] = as match {
    case Nil => Nil
    case Cons(h, t) => List.foldRight(h, flatten(t))((a,b) => Cons(a, b))
  }
}

object Exercise316 {
  def increment(as: List[Int]): List[Int] = List.foldRight(as, Nil: List[Int])((a,b) => Cons(a + 1, b))
}

object Exercise317 {
  def doubleToString(as: List[Double]): List[String] = List.foldRight(as, Nil: List[String])((a,b) => Cons(a.toString, b))
}

object Exercise318 {
  def map[A, B](as: List[A])(f: A => B): List[B] = List.foldRight(as, Nil: List[B])((a,b) => Cons(f(a), b))
}

object Exercise319 {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = List.foldRight(as, Nil: List[A])((a,b) => if(f(a)) Cons(a, b) else b)
}

object Exercise320 {
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = Exercise315.flatten(Exercise318.map(as)(f))
}

object Exercise321 {
  def filter[A](as: List[A])(f: A => Boolean): List[A] = Exercise320.flatMap(as)(h => if(f(h)) List(h) else Nil)
}

object Exercise322 {
  def sum(l: List[Int], m: List[Int]): List[Int] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2,t2)) => Cons(h1 + h2, sum(t1, t2))
  }
}

object Exercise323 {
  def zipWith[A,B,C](l: List[A], m: List[B])(f: (A, B) => C): List[C] = (l, m) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1, t2)(f))
  }
}

object Exercise324 {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def internalLoop(list: List[A], partial: List[A], acc: Boolean): Boolean = (list, partial) match {
      case (Nil, Nil) => acc
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h1,t1), Cons(h2, t2)) if h1 == h2 => internalLoop(t1, t2, true)
      case (Cons(h1,t1), Cons(h2, t2)) if h1 != h2 => internalLoop(t1, sub, false)
    }

    internalLoop(sup, sub, true)
  }
}

object Exercise325 {
  def size[A](as: Tree[A]): Int = as match {
    case Leaf(_) =>  1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
}

object Exercise326 {
  def max(as: Tree[Int]): Int = as match {
    case Leaf(v) => v 
    case Branch(l,r) => max(l) max max(r)
  }
}

object Exercise327 {
  def depth[A](as: Tree[A]): Int = as match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }
}

object Exercise328 {
  def map[A, B](as: Tree[A])(f: A => B): Tree[B] = as match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
}

object Exercise329 {
  def fold[A,B](as: Tree[A])(f: A => B)(g: (B,B) => B): B = as match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size[A](as: Tree[A]): Int = fold(as)( x => 1)((x,y) => 1 + x + y)
  def max(as: Tree[Int]): Int = fold(as)(x => x)((x,y) => x max y)
  def depth[A](as: Tree[A]): Int = fold(as)(x => 1)((x,y) => 1 + (x max y))
  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = {
    val leaf: A => Tree[B] = (x: A) => Leaf(f(x))
    val branch = (x: Tree[B], y: Tree[B]) => Branch(x, y)
    fold(as)(leaf)(branch)
  }
}