object exam1 {

  //Exercise 1

  lazy val from: (Int, Int) => List[Int] = (n, m) => {
    n match {
      case `m` => List(n)
      case _ => n :: from(n + 1, m)
    }
  }
  from(1, 4)


  //Exericse 2

  def from2[A](vFrom: A, vTo: A, gen: A => A): List[A] = {
    vFrom match {
      case `vTo` => List(vTo)
      case _ => vFrom :: from2(gen(vFrom), vTo, gen)
    }
  }

  from2(2, 10, (n: Int) => n + 2)
  from2('k', 'a', (n: Char) => (n.toInt - 1).toChar)


  //Exercise 3


  //Exercise 4

  lazy val polynomialAt: (List[Int] => (Int => Int)) = { coefficients => { x => {
    lazy val go: (List[Int], Int, Int) => Double = (coefficients, x, power) => {
      coefficients match {
        case Nil => 0
        case hd :: tl => coefficients.head * math.pow(x, power) + go(coefficients.tail, x, power + 1)
      }
    }
    go(coefficients, x, 0).toInt
  }
  }
  }

  polynomialAt(List(2, 3, 6, 3))(2)


  // Exercise 5


}