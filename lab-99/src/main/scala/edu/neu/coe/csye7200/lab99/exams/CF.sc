import edu.neu.coe.csye7200.lab99.exams.ConFrac

// CONSIDER use foldRight instead of (explicit) recursion.
// TODO get better precision than 1E-5
def calculate(xs: Seq[Int]): Double =xs.toList match {
 case Nil => 1
 case h :: t => h + 1 / calculate(t)
}

def calculateVarArgs(xs: Int*): Double = calculate(xs)

val result7 = calculateVarArgs(4, 2, 1, 3, 1, 2, 8)
if (math.abs(result7 - 4.358895705521473) < 1E-5) println("7 OK")

val result8 = calculate(LazyList(4, 2, 1, 3, 1, 2, 8))
if (math.abs(result8 - 4.358895705521473) < 1E-5) println("8 OK")

val result9 = calculate(LazyList.continually(1).take(9))
if (math.abs(result9 - 1.6181818181818182) < 1E-3) println("9 OK")

val result10 = ConFrac(LazyList.continually(1)).evaluate(1E-7)
if (math.abs(result10 - 1.618033988749895) < 1E-7) println("10 OK")

val ps1: LazyList[Int] = 4 #:: LazyList.continually(LazyList(2, 1, 3, 1, 2, 8)).flatten
val result11 = ConFrac(ps1).evaluate(1E-9)
if (math.abs(result11 * result11 - 19) < 1E-7) println("11 OK")