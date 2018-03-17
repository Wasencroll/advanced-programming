
object functions {
  // Functions
	(a: Int) => 2*a
	(a: Int, b: Int) => a*b
	(a: Int, f: Int=>Int) => f(a)
	
	((a: Int) => 2*a)(3)
	((a: Int, f: Int=>Int) => f(a))(3, ((a:Int) => 2*a))

	
	val f1 = (a: Int) => 2*a
	val f2 = (a: Int, b: Int) => a*b
	val f3 = (a: Int, f: Int=>Int) => f(a)
	
	f1(3)
	f2(3, 2)
	f3(3, x=>x*2)
	f3(3, f1)
	
	val f11: Int => Int = a => 2*a
	val f12: Int => Int = (a: Int) => 2*a
	val f21: (Int, Int) => Int = (a,b) => a*b
	val f22: (Int, Int) => Int = (a: Int, b: Int) => a*b

  val f31: (Int, Int=>Int) => Int = (a, f) => f(a)
	val f32: (Int, Int=>Int) => Int = (a: Int, f: Int=>Int) => f(a)

	
	f31(3, f11)
	
	// Higher-order functions
	val qam: (Double, Double, Double=>Double, Double=>Double) => Double = (a, b, f, fm1) => fm1((f(a)+f(b))/2)


	qam(1,2, Double=>Double, Double=>Double)
	qam(1,2, (x:Double)=>x*x, (x:Double)=> Math.sqrt(x))

	
	// Curryfication
	val am: (Double, Double) => Double = (a,b) => (a+b)/2

	val curryAm: Double => (Double => Double) = (a) => { (b) => (a+b)/2 }

	
	am(2,5)
	curryAm(2)(5)
	curryAm(2)
	val meanWith2 = curryAm(2)
	meanWith2(10)
	
	val compoundInterest: (Double, Double, Double) => Double = (i, t, p) => p*Math.pow(1+i, t)

	val curryCompoundInterest: Double => (Double => (Double => Double)) = (i) => { (t) => { (p) =>  p*Math.pow(1+i, t) }}


	val standardInterest = curryCompoundInterest(0.025)

	compoundInterest(0.025, 5, 1000)
	curryCompoundInterest(0.025)(5)(1000)
	standardInterest(5)(1000)
	
	// Recursion
	lazy val fact: (Int => Int) = (n) => {
		if (n == 0) {1}
		else {n * fact(n-1)}
	}
	
	lazy val fact2: (Int=>Int) = (n) => {
		var res = 1
		for (i <- 1 to n) {
			res = res*i
		}
		res
	}
	
	fact2(3)
	
	lazy val fibonacci: Int => Int = (n) => {
		if (n == 0) 1
		else if (n==1) 1
		else fibonacci(n-1) + fibonacci(n-2)
	}
	
	fibonacci(2)
	fibonacci(5)
	
	
}