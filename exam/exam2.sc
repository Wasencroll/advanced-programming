object exam2 {

  //Exercise 1

  def from(n: Int, m: Int): List[Int] = {
    n match {
      case `m` => List(n)
      case _ => n :: from(n + 1, m)
    }
  }

  from(1, 4)


  //Exercise 2

  def pascal(current: List[Int]): List[Int] = {
    lazy val newRow: List[Int] => List[Int] = (current) => {
      current match {
        case Nil => Nil
        case hd :: Nil => 1 :: Nil
        case hd :: tl => hd + tl.head :: newRow(tl)
      }
    }
    1 :: newRow(current)
  }

  pascal(List())
  pascal(List(1))
  pascal(List(1, 1))
  pascal(List(1, 2, 1))


  //Exercise 3

  def interleave[A](s1: Stream[A], s2: Stream[A]): Stream[A] = {
    s1.head #:: s2.head #:: interleave(s1.tail, s2.tail)
  }

  interleave(Stream(1, 2, 3, 4), Stream(10, 20, 30, 40)).take(5) foreach println


  //Exercise 4

  def append(listA: List[Int], listB: List[Int]): List[Int] = {
    listB match {
      case Nil => listA
      case hd :: tl => append(listA ::: List(hd), tl)
    }
  }

  append(List(1, 2), List())


  //Exercise 5

  lazy val curryFrom: (Int => (Int => List[Int])) = { n => { m => {
    n match {
      case `m` => List(n)
      case _ => n :: curryFrom(n + 1)(m)
    }
  }
  }
  }
  val fromTwo = curryFrom(2)
  val to20 = fromTwo(20)


}