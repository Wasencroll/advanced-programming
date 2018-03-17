
/**
 * Some warm up exercises on:
 * - Recursion.
 * - Recursion with lists.
 * - Pattern matching.
 * - Anonymous functions
 * - Higher order functions.
 *
 * One rule: Only immutable variables.
 */
object basic {
  type ??? = Nothing

  /**
   * -- Exercise 1 --
   * Create a "sum" function that receives two Int parameters.
   * The function should return the sum of all integers within the given range.
   * Test:
   * - sum(1, 3) = 6
   * - sum(5, 10) = 45
   */
  lazy val sum: (Int, Int) => Int = (f, t) => {
  	if (f == t) {
  		f
  		}
  	else {
  		f + sum (f+1, t)
  	}
  }
  
  sum(1,3)
  sum(5,10)

  /**
   * -- Exercise 2 --
   * Create a function "fact" that computes the factorial of a given number "n".
   * Test:
   * - fact(3) = 6
   * - fact(5) = 120
   * - fact(7) = 5040
   */
  lazy val fact: (Int => Int) = (n) => {
   if (n == 0) {
   	1
   }
   else {
   	n * fact(n-1)
   }
  }
  
  fact(3)
  fact(5)
  fact(7)

  /**
   * -- Exercise 3 --
   * Create a function "square" that receives a list integers "ns" and
   * returns a new list with the squares of the values given in "ns"
   * Use pattern matching.
   * Test:
   * - List(1, 2, 3, 4, 5) = List(1, 4, 9, 16, 25)
   * - List(12, 56, 32) = List(144, 3136, 1024)
   */
  lazy val square: (List[Int] => List[Int]) = (l) => {
  	l match {
 			case Nil => List()
  		case hd::tl => hd*hd :: square(tl)
  	}
  }
  
  
  square(List(1,2,3,4,5))
  square(List(12,56,32))

  /**
   * -- Exercise 4 --
   * Create a function "map" which receives a list of integers "ns" and a function "f"
   * and returns a new list with values of "ns" transformed by "f".
   * Test (use anonymous functions):
   * - Squares: List(1, 2, 3, 4, 5) = List(1, 4, 9, 16, 25)
   * - Cubes: List(1, 2, 3, 4, 5) = List(1, 8, 27, 64, 125)
   */
  lazy val map: (List[Int], Int => Int) => List[Int] = (ns, f) => {
  	if (ns == Nil) {
  		List()
  	}
  	else {
  		f(ns.head) :: map(ns.tail, f)
  	}
  
  }
  
  val sq = (n:Int) => n*n
  val cu = (n:Int) => n*n*n
  
  map(List(1,2,3,4,10), n => n*n)
  map(List(1,2,3,4,5), cu)
                                                  
  /**
   * -- Exercise 5 --
   * Create a function "filter" that receives a list of integers "ns" and a function "include", and
   * returns a new list with only the values of "ns" which passes the "include" test.
   * Test (using anonymous functions):
   * - Even numbers: List(1, 2, 3, 4, 5) = List(2, 4)
   * - Multiples of 3: List(9, 8, 7, 6, 5, 4) = List(9, 6)
   * - Ages between 20 and 65: List(60, 23, 11, 76, 42, 9) = List(60, 23, 42)
   */
  lazy val filter: (List[Int], Int => Boolean) => List[Int] = (ns, include) => {
  	if (ns == Nil) {
  		List()
  	}
  	else {
  		if (include(ns.head) == true) {
  			ns.head :: filter(ns.tail, include)
  		}
  		else {
  			filter(ns.tail, include)
  		}
  	}
  }
  
  val even: (Int => Boolean) = (n) => n%2 == 0
  val times3: (Int => Boolean) = (n) => n%3 == 0
  val age20to65 = (n:Int) => 20 <= n && n <= 65
  
  filter(List(1,2,3,4,5), even)
  filter(List(9,8,7,6,5,4), times3)
  filter(List(60,23,11,76,42,9), age20to65)
}