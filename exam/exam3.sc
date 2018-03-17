object exam3 {


  //Exercise 1 - n combinations of {0,1}

  lazy val combination: Int => List[List[Int]] = (n) => {
    lazy val expand: (List[Int], Int) => List[List[Int]] = (list, n) => {
      list.length match {
        case `n` => List(list)
        case _ => expand(list ::: List(0), n) ::: expand(list ::: List(1), n)
      }
    }
    n match {
      case 0 => Nil
      case _ => expand(List(0), n) ::: expand(List(1), n)
    }
  }
  combination(3)


  //Exercise 2 - second Smallest element of a list

  lazy val secondSmallest: List[Int] => Int = (list) => {
    if (list.length < 2) throw new Exception("not enough elements in list")
    lazy val sorting: List[Int] => List[Int] = (list) => {
      list match {
        case Nil => Nil
        case hd :: tl => sorting(tl.filter(_ <= hd)) ::: hd :: sorting(tl.filter(_ > hd))
      }
    }
    (sorting(list)).tail.head

  }
  secondSmallest(List(6, 4, 5, 1, 3, 9))


  //Exercise 3 - split streams based on separator into Stream[List[]]

  lazy val stream2Word: (Stream[Any], Any => Boolean) => Stream[List[Any]] = (s, p) => {
    s match {
      case Stream.Empty => Stream.Empty
      case _ => (s.takeWhile { n => n != ' ' }).toList #:: stream2Word(s.tail.tail, p)
    }
  }

  val result = stream2Word(('H' #:: 'I' #:: ' ' #:: 'Y' #:: 'O' #:: 'U' #:: Stream.Empty), (n: Any) => n == ' ').take(4) foreach println


  //Exercise 4 - append of listA to listB using higher-order functions (map, reduce, fold like functions)

  /**
    * lazy val append: (List[Int], List[Int]) => List[Int] = (listA, listB) => {
    * listA match {
    * case Nil => listB
    * case hd::tl => listB match {
    * case Nil => listA
    * case hd::tl => append(listA ::: List(listB.head), listB.tail)
    * }
    * }
    * }
    * append(List(1,2,3), List(4,5,6))
    **/

  lazy val append = ???


  //Exercise 5 - discuss currification and give an example


}