import java.time.LocalTime
// Partial functions
val f = (x: Int) => x * x +1
f(3)
val h: Int => String => Char =
  (x: Int) => (y: String) => y.charAt(x)
h(3)("hello")
val h3 = h(3) // partially applied curried function called h3
h3("hello")
val jAsAFunction: (Int,String) => Char =
  (x: Int, y: String) => y.charAt(x)
def jAsAMethod(x: Int, y: String): Char =
  y.charAt(x)
val myTuple = (4,"hello")
jAsAFunction.tupled(myTuple)
val partialFunction = jAsAFunction(_,"hello") // partially applied uncurried function
partialFunction(3)
val g: PartialFunction[(Int,String),Char] =
{
  case (x,_) if x < 0 => '?'
  case (x,y) if x >= y.length => '?'
  case (x,y) => y.charAt(x)
}
g(3,"hello")
g(-1,"hello")
g(7, "hello")
def logTime(b: Boolean, time: => String): Unit =
  if (b) println(time)
def getTime: String = {
  println("getTime: sleeping for 5 second")
  Thread.sleep(5000)
  LocalTime.now().toString
}
logTime(false, getTime)
println("all done")

