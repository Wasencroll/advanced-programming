
object exercises {
  /* Exercise 1: Define a recursive version of the function from(n,m) with n and m
		integers. The function returns the list of integers from n to m. Assume n ≤ m.
		Consider its implementation using pattern matching. */
		
	lazy val from: (Int, Int) => List[Int] = (n, m) => {
		(n-m) match {
			case 1 => List()
			case _ => (n) :: from(n+1, m)
		}
	}
	
	from(2,5)
	
	
	
	/* Exercise 2. Define a recursive version of the method from(vFrom,vTo,gen)
		which generates a list of elements where the first one is vFrom and the last
		one is vTo. These two elements are of an arbitrary type. Then, gen is a function
		that given an element of this arbitrary type generates a new one.
		The function from starts with vFrom and generates elements with gen until
		vTo is reached.
		Give examples of its application to generate lists of integers from 3 to 10,
		even numbers from 2 to 10, characters from a to k, and powers of 2 from 16 to
		256.
		Note. As from is polymorfic because we can define it for an arbitrary type,
		we need to define it using def.*/
	
	def from2[A](vFrom: A, vTo: A, gen: A => A): List[A] =
		(vFrom, vTo) match {
			case (`vTo`, `vTo`) => List(vTo)
			case _ => vFrom :: from2(gen(vFrom), vTo, gen)
		}
		
		from2(2, 10, (x:Int) => x+2)
		from2(3, 10, (x:Int) => x+1)
		from2('a', 'k', (x:Char) => (x.toInt+1).toChar)

		from2(16, 256, (x:Int) => x*2)

		
	
	/* Exercise 3. Given a row of Pascal’s triangle (or Tartaglia’s triangle) build a new
		row. Give a recursive version and a version using higher-order functions (i.e.,
		using map, reduce like functions).
		Recall that given the row
			[a1, a2, a3, . . . an−1, an]
		the new row of the triangle will be
			[1,(a1 + a2),(a2 + a3),(a3 + a4), . . .(an−1 + an), 1]. */
	
	lazy val pascal1: List[Int] => List[Int] = (l) => {
		lazy val nextRow: List[Int] => List[Int] = (l) => {
			l match {
				case Nil => 1::Nil
				case hd :: Nil => 1::Nil
				case hd::tl => (hd+tl.head) :: nextRow(tl)
			}
		}
		1::nextRow(l)
	}
	pascal1(List(1))
	pascal1(List(1,1))
	
	lazy val pascal2: List[Int] => List[Int] = (l) => {
		1::l.zip(l.tail:::List(0)).map((a)=>a._1+a._2)
	}
	pascal2(List(1))
	pascal2(List(1,1))
	
	/* Exercise 4. Defne the function append that given two lists of integers returns
		their concatenation. Give a recursive version and a version using higher-order
		functions (i.e., using map, reduce like functions). */
		
	lazy val append: (List[Int], List[Int]) => List[Int] = (xn, xs) => {
		xn match {
			case Nil => xs
			case hd::tl => xs match {
				case Nil => xn
				case hd::tl => append(xn::: List(xs.head), xs.tail)
			}
		}
	}
	append(List(1,2,3), List(4,5,6))
	
	def appendX: ((List[Int], List[Int]) => List[Int]) = (listA, listB) => listB match {
  	case Nil => listA
  	case hd::tl => appendX(listA:::List(hd), tl)
	}
	appendX(List(1,2,3), List(4,5,6))
	
	
	
	/* Exercise 5. Defne a currifed version of the function curryF that returns
		a * (b + c)^2
		The defnition should be done so that the following expressions work correctly.
		curryF (2)(3)(1)
		List(1.0,2.0,3.0).map(curryF(2)(3))*/
		
		lazy val curryF: (Double => (Double => (Double => Double))) = {a => {b => {c => a*(b+c)*(b+c)}}}

		curryF(2)(3)(1)
		List(1.0,2.0,3.0).map(curryF(2)(3))

                                                  
	/* Defne the function quicksort that given a list of integers, returns
		the list of integers ordered (from lower to large). Give a recursive version using
		pattern matching.*/
		
		lazy val quicksort: List[Int] => List[Int] = (l) => {
			l match {
				case Nil => Nil
				case hd::tl => quicksort(tl.filter(_ <= hd)):::hd::quicksort(tl.filter(_ > hd))
			}
		}
		quicksort(List(1,4,8,3,1,77,-100,(21/3)))



	/*Exercise 7. Define another version of the function quicksort with two arguments.
		The first one is a list of elements of an arbitrary type, and the second
		a function that given two elements of this type returns true when the first is
		smaller than the second. Then, quicksort returns the list of elements ordered
		(from lower to large according to the function).
		Note. As the function is polymorphic, needs to be defned using def.*/
		
		def quicksort2[A](listA: List[A], f: (A, A) => Boolean): List[A] = {
			listA match {
				case Nil => Nil
				case hd::tl => quicksort2(tl.filter(f(_, hd)), f):::hd::quicksort2(tl.filter(f(hd,_)), f)
			}
		}
		quicksort2(List(4,1,7,8,6,-1,2,-10,-11), (a:Int,b: Int) =>(a<b))


		
	/* Exercise 8 */
		
		lazy val monomial: ((Double, Double) => (Double => Double)) = { (a, e) => { x => {
			a match {
				case 0 => 0
				case _ => e match {
					case 0 => 1
					case _ => a*Math.pow(x, e)
				}
			}
		}}}

		val a=1
		val e=2
		val setOfValues = Set(1,2,3,2.2)

		//setOfValues monomial(a, e)

	
	/* Exercise 9 */
	
		lazy val polynomialAt: (List[Double] => (Double => Double)) = {a => {x => {
			lazy val go: (List[Double], Double, Double) => Double = (a, x, e) => {
				a match {
					case Nil => 0
					case hd::tl => hd*math.pow(x, e) + go(tl, x, e+1)
				}
			}
			go(a, x, 0)
		}}}
		polynomialAt(List(2,3,6,3))(2)
	
	
	/* Exercise 10 */
	
		lazy val polynomialCompactAt: (List[Double] => (List[Double] => (Double => Double ))) = { a => { e => { x => {
			a match {
				case Nil => 0
				case hd :: tl => hd*math.pow(x, e.head) + polynomialCompactAt(tl)(e.tail)(x)
			}
		}}}}

		polynomialCompactAt(List(2,3,6,3))(List(1,2,3,4))(2)

		
		lazy val polynomialCompactAt2: (List[Double] => (List[Double] => (Double => Double ))) = { a => { e => { x => {
			a.zip(e).map( (t:(Double, Double)) => t._1*math.pow(x, t._2)).foldLeft(0.0)( (a,b) => a+b )
		}}}}

		polynomialCompactAt2(List(2,3,6,3))(List(1,2,3,4))(2)



	/* Exercise 11 */
	
	
	
	/* Exercise 15 */
	
	lazy val movAvg: Stream[Int] => Int = (s) => { s match {
		case isEmpty => 1
		case _ => (1/3)* (s.head + movAvg(s.tail))
	}}
	movAvg(Stream(1,10,20))
	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
}