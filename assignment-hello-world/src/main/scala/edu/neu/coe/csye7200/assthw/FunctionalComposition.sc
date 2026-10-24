import java.io.PrintWriter

val f: Int => Int = _ + 1

val g: Int => Double = _ * 2.0

val h: Int => Double = f andThen g

val k: Int => Double = h

def m(x: Int, y: Double): Double = x + y

val fm: (Int, Double) => Double = m

val cfm: Int => Double => Double = (m _).curried

val tfm: ((Int, Double)) => Double = fm.tupled

tfm((1, 3.1415927))

cfm(1)(3.1415927)

fm(1, 3.1415927)

m(1, 3.1415927)

def log(pw: PrintWriter)(s: String): Unit = pw.println(s)

val writerFunction: String => Unit = log(new PrintWriter(System.out, true))

def logAll(f: String => Unit)(ss: String*): Unit = ss.foreach(
  s => f(s)
)

val strings = Seq("Hello, world!", "Goodbye, Mr. Chips!", "Haskell Brooks Curry")

logAll(writerFunction)(strings: _*)

def myLength(s: String): Int = s.length

strings map myLength