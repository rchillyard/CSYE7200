package edu.neu.coe.csye7200.lab99.exams

case class ConFrac(xs: LazyList[Int]) {

  def convergents: LazyList[Convergent] = xs.scanLeft(Convergent.minusOne)((c, x) => c.next(x)).drop(1)

  def evaluate(n: Int): Double = convergents.take(n).last.toDouble

  def evaluate(epsilon: Double): Double = convergents.takeWhile(c => c.k.bitCount < -math.log(epsilon) / math.log(2)).last.toDouble
}

case class Convergent(h: BigInt, k: BigInt, maybePrevious: Option[Convergent]) {
  def asTuple: (BigInt, BigInt) = h -> k

  def toDouble: Double = h.toDouble / k.toDouble

  def next(a: Int): Convergent = maybePrevious match {
    case Some(Convergent(v, w, _)) => Convergent(a * h + v, a * k + w, Some(this))
    case _ => throw new Exception("logic error")
  }
}

object Convergent {
  val minusOne: Convergent = Convergent(1, 0, Some(Convergent(0, 1, None)))
}