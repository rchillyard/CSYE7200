import scala.util.Random

val directory = Map("Alice" -> 3, "Bob" -> 1, "Carol" -> 2)
val names = List("Alice", "Bob", "Carol")
val phones = List(1, 2, 3)

def matchUp(name: String, phone: Int): Option[(String, Int)] =
  for (n <- names.find(_ == name); x <- phones.find(_ == phone); z <- directory.find(_ == (n,x))) yield z

matchUp("Alice", 3)
matchUp("Alice", 2)
matchUp("Robin", 2)
matchUp("Alice", 0)
matchUp("", 0)

val directory = Map("Alice" -> 3, "Bob" -> 1, "Carol" -> 2)
val names = List("Alice", "Bob", "Carol")
val phones = List(1, 2, 3)


//def matchUp(name: String, phone: Int): Option[(String, Int)] =  directory.find( _ == (names.find(_ contains(name)).getOrElse("Name not found"),phones.find(_ == phone).getOrElse(0)))

def matchUp(name: String, phone: Int): Option[(String, Int)] =
  directory.find({case (name,phone) => directory(name) == phone})

matchUp("Alice", 3)
matchUp("Robin", 2)
matchUp("Alice", 0)
matchUp("", 0)

val f: LazyList[BigDecimal]  =
  BigDecimal(1) #:: f.map(x => 1.0 / (x*x))


LazyList.from(1) zip f take 1000 foreach println

def pi(current: Int = 1, next: Int = 1/2): LazyList[Int] =
  LazyList.cons(current, pi(next, current + next))

(pi() take 1000).toList

val terms: LazyList[Double] = 1 #:: terms.map{ n => 1/ ((n+1) * (n+1)) }
def calPi(n:Double) : Double = math.sqrt(n * 6)
val xs = terms take 3
xs.toList
xs.toList.sum

def calculatePi(loops: Int)(n: Int)(implicit r: Random): Double =
{

  var pi: Double = 0
  var n = 0 {
    val loops = readInt()
    val pi = calculatePi(loops)
  }
  while (n < loops)
  {
    pi = pi + (pow (- 1, n) / (2 * n + 1) )
    n = n + 1
  }
  pi * 4
}
println