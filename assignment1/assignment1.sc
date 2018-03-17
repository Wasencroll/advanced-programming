object assignment1 {



/**
  * -- Exercise 1 --
  *	Implement a recursive function with the name combination that returns a list with combinations of 0 and 1.
  * Test:	combination(2) = List(List(0, 0), List(0, 1), List(1, 0), List(1, 1))
  *				combination(3) = List(List(0, 0, 0), List(0, 0, 1), List(0, 1, 0), List(0, 1, 1),
  *															List(1, 0, 0), List(1, 0, 1), List(1, 1, 0), List(1, 1, 1))
  *
  *	Define the most general case where all possible combinations from a list of
  *	digits is considered. Show examples of their use.
  *	Test:	combinationList(2,List(0,1,2)) = List(List(0, 0), List(0, 1), List(0, 2),
  *																							List(1, 0), List(1, 1), List(1, 2),
  *																							List(2, 0), List(2, 1), List(2, 2))
  *
  *					http://stackoverflow.com/questions/8131291/how-to-convert-an-int-to-a-string-of-a-given-length-with-leading-zeros-to-align
  */


/**
  * -- Function 1: combination --
  */
lazy val combination: Int => List[List[Int]] = (n) => {

  if (n < 0 ) throw new Exception("Negative number of combinations not possible!")

  lazy val add: (Int, List[Int]) => List[List[Int]] = (n, inputList) => {
    n match {
      case 0 => List(inputList)
      case _ => add(n-1, inputList ::: List(0)) ::: add(n-1, inputList:::List(1))
    }
  }
  lazy val combine: (Int, List[List[Int]]) => List[List[Int]] = (n, inputList) => {
    inputList match {
      case Nil => Nil
      case _ => add(n-1, inputList.head) ::: combine(n, inputList.tail)
    }
  }
  n match {
    case 0 => List()
    case _ => combine(n, List(List(0), List(1)))
  }
}

/** Alternative with built-in functions **/
/*
  lazy val altCombination: Int => List[List[Any]] = (limit) => {
    if (limit<0) throw new Exception("Negative number of combinations not possible!")
    lazy val createCombinations: Int => List[List[Any]] = (n) => {
      n match {
        case 0 => List(n.toBinaryString.reverse.padTo(limit, 0).reverse.toList)
        case _ => createCombinations(n-1) ::: List(n.toBinaryString.reverse.padTo(limit, 0).reverse.toList)
      }
    }
    limit match {
      case 0 => List()
      case _ => createCombinations((math.pow(2, limit).toInt -1))
    }
  }
*/

/** -- Testing Function 1 -- **/
combination(2)
combination(3)
combination(0)


/**
  * -- Function 2: combinationList			combinationList(2, List(0, 1, 2))
  */

val combinationList: (Int, List[Any]) => List[List[Any]] = (depth, inputList) => {
  if (depth < 0) throw new Exception("Negative number of combinations not possible!")

  lazy val mapToInput: (Int, List[Any]) => Any = (n, inputList) => {
    if (n == 0) {
      inputList.head
    }
    else {
      mapToInput(n - 1, inputList.tail)
    }
  }
  lazy val add: (Int, Int) => List[Any] = (depth, limit) =>	{
    if (depth > 1) {
      add(depth - 1, limit/inputList.length) ::: List(mapToInput(limit%inputList.length, inputList))
    }
    else {
      List(mapToInput(limit%inputList.length, inputList))
    }
  }
  lazy val makeOutput: (Int, Int) => List[List[Any]] = (depth, limit) => {
    if (limit < math.pow(inputList.length, depth).toInt) {
      List(add(depth, limit)) ::: makeOutput(depth, limit+1)
    }
    else {
      Nil
    }
  }
  makeOutput(depth, 0)
}



/**		lazy val combinationList: (Int, List[Any]) => List[List[Any]] = (depth, inputList) => {
			if (depth < 0) throw new Exception("Negative number of combinations not possible!")

			depth match {
				case 0 => List()
				case 1 => inputList match {
											case Nil => Nil
											case hd :: tl => 	combinationList(depth, inputList.tail) ::: List(List(hd))
									}
				case _ => inputList match {
											case Nil => List()
											case hd :: Nil => combinationList(depth-1, inputList) ::: List(List(hd))
											//case hd :: tl => combinationList(depth-1, inputList) ::: List(List(hd))
									}
			}

		} **/

/**
  * -- Testing Function 2 --
  */

combinationList(1, List(0, 1, 2))
combinationList(2, List(4))
combinationList(2, List(7, 8, "a"))



/**
  * -- Exercise 2 --
  * Implement the function baseChange recursively.
  * Test: baseChange (4532,10) = List(4, 5, 3, 2)
  *				baseChange (45,2) = List(1, 0, 1, 1, 0, 1)
  */

val baseChange: (Int, Int) => List[Int] = (n, b) => {

  if (n < 0) throw new Exception("Your input is negative!")
  if (b <= 1) throw new Exception("Enter a base greater than 1!")

  n match {
    case 0 => List(0)
    case _ => lazy val change: (Int, Int) => List[Int] = (n, b) => {
      n match {
        case 0 => List()
        case _ => change(n/b, b) ::: List(n % b)
      }
    }
      change(n, b)
  }
}


/** -- Testing Exercise 2 -- **/
baseChange(4532, 10)
baseChange(45, 2)
baseChange(0, 4)
baseChange(2, 1)


}