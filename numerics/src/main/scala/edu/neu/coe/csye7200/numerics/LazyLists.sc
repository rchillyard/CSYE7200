import LazyList._

val positiveIntegers: LazyList[Int] = from(1)
val firstTen: LazyList[Int] = positiveIntegers take 10
firstTen to List

// Fibonacci
val f: LazyList[BigInt] = BigInt(0L) #:: f.scanLeft(BigInt(1L))(_ + _)

val firstTwenty = f take 20 to List
firstTwenty foreach  println

val g: LazyList[Long] = 0L #:: 1L #:: g.zip(g.tail).map (n => n._1 + n._2)
val firstThirty = g take 30 to List

