
object statements {
  // Statements
  if (4 < 5) { "Hello World" }
  else { "Goodbye" }

  {
    var i = 0
    while (i < 4) { "Hello World"; i = i + 1 }
  }

  for (i <- 1 to 10) { "Hello World" }

  // Declarations
  val a1 = 2 * 5

  val c = {
    val c1 = 10
    val c2 = "In text: " + c1 + " is ten"
    (c1, c2)
  }

  // Cartesian Product
  val b1 = (2 * 5, "ten")
  b1._1
  b1._2
}