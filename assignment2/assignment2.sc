object assignment2 {


  // Binary Tree
  trait Tree[+A]

  case object EmptyTree extends Tree[Nothing]

  case object add extends Tree[Nothing]

  case object mult extends Tree[Nothing]

  case class Add[A](root: Tree[A], left: Tree[A], right: Tree[A]) extends Tree[A]

  case class Leaf[A](element: A) extends Tree[A]

  case class Var[A](element: A) extends Tree[A]


  // Implementing a class for Complex datatype
  class Complex(numRe: Double, numIm: Double) {
    var re: Double = numRe
    var im: Double = numIm

    override def toString = re + (if (im < 0) " - " + im + "i" else " + " + im + "i")

    def +(r2: Complex): Complex = new Complex(re + r2.re, im + r2.im)

    def *(r2: Complex): Complex = new Complex(re * r2.re - im * r2.im, re * r2.im + im * r2.re)
  }

  // Implementing trait for type class DataTypes[A]
  trait DataTypes[A] {
    def add(x: A, y: A): A

    def mult(x: A, y: A): A
  }

  // Implementing companion objects which implement specific data types with functions of add/multiply operations
  implicit object IntegerType extends DataTypes[Integer] {
    def add(x: Integer, y: Integer): Integer = x + y

    def mult(x: Integer, y: Integer): Integer = x * y
  }

  implicit object DoubleType extends DataTypes[Double] {
    def add(x: Double, y: Double): Double = x + y

    def mult(x: Double, y: Double): Double = x * y
  }

  implicit object ComplexType extends DataTypes[Complex] {
    def add(x: Complex, y: Complex): Complex = x + y

    def mult(x: Complex, y: Complex): Complex = x * y
  }

  //...implement more objects of DataType, e.g. Byte, Short, Long...

  // Assigning 'add' & 'mult' to the respective operators '+' & '*'
  implicit class Operators[A](lhs: A)(implicit num: DataTypes[A]) {
    def +(rhs: A): A = num.add(lhs, rhs)

    def *(rhs: A): A = num.mult(lhs, rhs)
  }

  // function pre-order
  def preorder[A](tree: Tree[A]): List[Any] = tree match {
    case Add(root, left, right) => root match {
      case `add` => List(add) ::: preorder(left) ::: preorder(right)
      case `mult` => List(mult) ::: preorder(left) ::: preorder(right)
    }
    case Leaf(value) => value match {
      case x: Int => "i" + x :: Nil
      case _ => value :: Nil
    }
    case Var(root) => throw new Exception("no variables supported")
  }


  // function in-order
  def inorder[A](tree: Tree[A]): List[Any] = tree match {
    case Add(root, left, right) => root match {
      case `add` => inorder(left) ::: List(add) ::: inorder(right)
      case `mult` => inorder(left) ::: List(mult) ::: inorder(right)
    }
    case Leaf(value) => value match {
      case x: Int => "i" + x :: Nil
      case _ => value :: Nil
    }
    case Var(root) => throw new Exception("no variables supported")
  }


  // function evaluate
  def evaluate[A: DataTypes](tree: Tree[A]): String = {
    lazy val calc: (Tree[A] => A) = tree => tree match {

      case Add(root, left, right) => root match {
        case `add` => calc(left) + calc(right)
        case `mult` => calc(left) * calc(right)
      }
      case Leaf(value) => value
      case Var(root) => throw new Exception("no variables supported")
    }
    calc(tree) match {
      case x: Int => "i" + x
      case any => any.toString
    }
  }

  // Tree Definition from the Exercise
  val example: Tree[Integer] = Add(mult,
    Add(add, Leaf(new Integer(4)), Leaf(new Integer(7))),
    Add(mult, Add(add, Leaf(new Integer(2)), Leaf(new Integer(1))), Leaf(new Integer(5))))

  // Tree Definition with Complex
  val example2: Tree[Complex] = Add(add,
    Add(mult, Leaf(new Complex(2, 3)), Leaf(new Complex(1, 2))),
    Add(add, Leaf(new Complex(1, 4)), Leaf(new Complex(3, 9))))

  val example3: Tree[Complex] = Add(add, Leaf(new Complex(3, 2)), Leaf(new Complex(1, 7)))

  // Testing
  evaluate(example2)
  preorder(example)
  inorder(example)
  inorder(example2)
  inorder(example3)
  evaluate(example3)


}