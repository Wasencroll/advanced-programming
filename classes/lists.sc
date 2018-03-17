
object lists {
  val exampleEmptyList = Nil
  val exampleListOne = 1::Nil
  val exampleListThree = 1::2::4::3::Nil
  val AnotherListOfThree = List(1,2,3)
  
  "First"::"Second"::Nil
  "First"::1::"Second"::2::Nil
  
  exampleListThree.head
  exampleEmptyList.isEmpty
  exampleListThree != AnotherListOfThree
  exampleListThree.last
  
  lazy val length: List[Int] => Int = (l) => {
  	if (l == Nil) 0
  	else 1+length(l.tail)
  }
  lazy val sum: List[Int] => Int = (l) => {
  	if (l == Nil) 0
  	else l.head+sum(l.tail)
  }
  lazy val prod: List[Int] => Int = (l) => {
  	if (l == Nil) 1
  	else l.head*prod(l.tail)
  }
  length(List(1,2,3,4))
  sum(List(1,2,7))
  prod(List(2,4,2))
  
  lazy val thereIs: (List[Int], Int) => Boolean = (l, n) => {
  	if (l == Nil) false
  	else {
  		if (n == l.head) true
  		else thereIs(l.tail, n)
  	}
	}
  thereIs(List(1,2,3), 3)
  
  lazy val thereIsOneSatisfyingP: (Int => Boolean, List[Int]) => Int = (p, l) => {
  	if (l == Nil) -1
  	else {
  		if (p(l.head)) l.head
  		else thereIsOneSatisfyingP(p, l.tail)
  	}
  }
  val multipleOf2: Int => Boolean = (x) => x%2==0
  val multipleOf3: Int => Boolean = (x) => x%3==0
  
  thereIsOneSatisfyingP(multipleOf2, List(1,3,5,6))

  thereIsOneSatisfyingP(multipleOf3, List(2,2,3))
}