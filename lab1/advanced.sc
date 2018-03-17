
/**
 * Some a bit more advanced(?) exercises for practicing:
 * - Recursion.
 * - Recursion with lists.
 * - Pattern matching.
 * - Anonymous functions
 * - Higher order functions.
 *
 * One rule: Only immutable variables.
 */
object advanced {
  type ??? = Nothing

  /**
   * -- Exercise 1 --
   * Create a function "fibonacci" which returns the value of a given position
   * within the fibonacci sequence.
   * https://en.wikipedia.org/wiki/Fibonacci_number
   * Test:
   * - 4 = 3
   * - 7 = 13
   * - 12 = 144
   */
  lazy val fibonacci: (Int => Int) = (n) => {
  	if (n == 1) {
  		1
  	}
  	else if (n == 2) {
  		1
  	}
  	else {
  		fibonacci(n-1) + fibonacci(n-2)
  	}
  }                                               //> fibonacci: => Int => Int
  
  fibonacci(4)                                    //> res0: Int = 3
  fibonacci(7)                                    //> res1: Int = 13
  fibonacci(12)                                   //> res2: Int = 144
  
  
  /**
   * -- Exercise 2 --
   * Create a function "reduce" which receives a list of integers "ns" and a function "combine"
   * and returns an integer resulting from having combined all values of "ns".
   * Test (use anonymous functions):
   * - Sum: List(1, 8, 4, 3, 9, 5) = 30.
   * - Max: List(23, 76, 34, 84, 24, 58) = 84.
   */
  /**
  lazy val reduce: (List[Int], (Int, Int) => Int) => Int = (ns, combine) => {
  	if (ns == Nil) {
  		0
  	}
  	else {
  		combine(ns.head, 0) + reduce(ns.tail, combine)
  	}
  
  }
  */
  
  lazy val reduce1: (List[Int], (Int, Int) => Int) => Int = (ns, combine) =>
  	ns match {
  		case Nil => throw new Exception("a")
  		case h :: Nil => h
  		case a :: t => combine(a, reduce1(t, combine))
  	}                                         //> reduce1: => (List[Int], (Int, Int) => Int) => Int
  
  val max: ((Int, Int) => Int) = (n, m) => if (n<m) m else n
                                                  //> max  : (Int, Int) => Int = <function2>
  val sum: ((Int, Int) => Int) = (n, m) => n+m    //> sum  : (Int, Int) => Int = <function2>
  
  reduce1(List(23,76,34,84,24,58), max)           //> res3: Int = 84
	//reduce(List(1,8,4,3,9,5), sum)
	

  /**
   * -- Exercise 3 --
   * Create a function "pascal" which receives two parameters, the first referring to a row number and
   * the latter to the column. These two are the coordinates to a value in the pascal triangle.
   * The function should return the value of the given coordinates.
   * https://en.wikipedia.org/wiki/Pascal%27s_triangle
   * Test:
   * - pascal(1, 1) = 1
   * - pascal(3, 2) = 2
   * - pascal(6, 4) = 10
   * - pascal(5, 5) = 1
   */
  lazy val pascal: (Int, Int) => Int = (r, c) => {
  	if (c == 1 || r == c) {
  		1
  	}
  	else {
  		pascal(r-1, c-1) + pascal(r-1, c)
  	}
  }                                               //> pascal: => (Int, Int) => Int
  
  pascal(1,1)                                     //> res4: Int = 1
  pascal(3,2)                                     //> res5: Int = 2
  pascal(6,4)                                     //> res6: Int = 10
  pascal(5,5)                                     //> res7: Int = 1

  /**
   * -- Exercise 4 --
   * Create a function which indicates whether a string containing parentheses is balanced.
   * Notes:
   * - You can use nested functions (use "lazy val" for nested functions).
   * - The String will be given in the form of a List of characters.
   * Test:
   * - "hello (world)".toList = true (balanced).
   * - "(()".toList = false (unbalanced).
   * - "()(())".toList = true.
   * - "(a)(b(c)()".toList = false.
   */
  lazy val balanced: (List[Char] => Boolean) = (l) => {
  	lazy val loop: (List[Char], Int) => Boolean = (chars, counter) => {
  		chars match {
  			case Nil => counter == 0
  			case '(' :: t => loop(t, counter+1)
  			case ')' :: t =>
  										if (counter > 0) loop(t, counter-1)
  										else false
  			case _ :: t => loop(t, counter)
  		}
  	}
  	loop( l, 0)
  }                                               //> balanced: => List[Char] => Boolean

balanced("hello (world)".toList)                  //> res8: Boolean = true
balanced("(()".toList)                            //> res9: Boolean = false
balanced("()(())".toList)                         //> res10: Boolean = true
balanced("(a)(b(c)()".toList)                     //> res11: Boolean = false

}