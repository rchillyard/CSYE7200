import edu.neu.coe.csye7200.fp.greedy.Fibonacci.{fibonacci, fibonacciBigInt}
import scala.annotation.tailrec

// Fibonacci

fibonacci(40) // 102334155

def tcoFibonacci(n: Int): BigInt = {
  @tailrec
  def inner(x: Int, a: BigInt, b: BigInt): BigInt = x match {
    case 0 => a
    case 1 => b
    case _ => inner(x - 1, b, a + b)
  }

  inner(n, 0, 1)
}

val n = 90 // last value for which we can evaluate Fibonacci as a Long.

fibonacci(n) // 2880067194370816120
fibonacciBigInt(n)
tcoFibonacci(n)

