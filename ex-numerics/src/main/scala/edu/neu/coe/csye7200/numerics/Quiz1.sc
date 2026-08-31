import edu.neu.coe.csye7200.prime.Prime.primes

val args = Array("12", "15", "20", "21", "22", "23", "24", "41")
val numbers: Array[Int] = args map (_.toInt)

def show(magicNumber: Int): String = {
  val ys: LazyList[BigInt] = primes.drop(2).map(p => (p.x * p.x) % magicNumber).take(100)

    ???

  s"""$magicNumber: ${ys.mkString(",")}"""
}

5 ^ 3

for (n <- numbers) println(show(n))
