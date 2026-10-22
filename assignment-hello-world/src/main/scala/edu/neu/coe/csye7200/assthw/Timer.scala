package edu.neu.coe.csye7200.assthw

trait Foldable[T]:
  def zero: T
  def combine: (T, T) => T

object Timer {

  extension [T: Foldable](n: Int)
    def times(f: => T): T =
      val start = System.nanoTime()
      val r: Seq[T] = for (_ <- 1 to n) yield f
      val end = System.nanoTime()
      val result = r.foldLeft(implicitly[Foldable[T]].zero)(implicitly[Foldable[T]].combine)
      println(s"${(end - start) / r.length / 1_000_000} millseconds per run")
      result
}
