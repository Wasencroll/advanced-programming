
object patternMatching {

	lazy val fact: Int => Int = (n) => {
		n match {
			case 0 => 1
			case m => m*fact(m-1)
		}
	}
	fact(5)
	
	lazy val lengthMatching: List[Int] => Int = (l) => {
		l match {
			case Nil => 0
			case hd::tl => 1+lengthMatching(tl)
		}
	}
	lengthMatching(1::2::3::Nil)

}