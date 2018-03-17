
object dataStructures {

	val x = 1 :: 2 :: 3 :: 4 :: Nil
	val y = x.head :: 20 :: x.tail.tail
	
	var a = 1 :: 2 :: 3 :: 4 :: Nil
	var b = a
	a = a.head :: 20 :: a.tail.tail
	println(a)
	
	val m = Array(1,2,3,4)
	val n = m
	m(1) = 20
	
	val oneToTen = 1 to 10

	val oneTo8 = 1 until 9
	
	val s1 = Set(1,2,3,4)
	val s2 = Set(3,4,5,6)
	s1.union(s2)
	s2.intersect(s1)
	
	val map1: Map[String, Int] = Map("one" -> 1)

	map1.get("one")
	map1 + ("two" -> 2)
	map1.head
	map1.isEmpty
}