package edu.neu.coe.csye7200


/**
  * Sum has been replaced by FutureExercise
  */
@deprecated
object Sum extends App {

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.language.postfixOps

  val chunk = 10000 // Try it first with chunk = 10000 and build up to 1000000
  def integers(i: Int, n: Int): LazyList[Int] = LazyList.from(i) take n

  def sum[N: Numeric](is: LazyList[N]): BigInt = is.foldLeft(BigInt(0))(_ + implicitly[Numeric[N]].toLong(_))

  def asyncSum(is: LazyList[Int]): Future[BigInt] = Future {
    val x = sum(is)
    System.err.println(s"${is.head} is done with sum $x")
    x
  }

  // CONSIDER using traverse
  val xfs: IndexedSeq[Future[BigInt]] = for (i <- 0 to 9) yield asyncSum(integers(i * chunk + 1, chunk))
  val xsf: Future[IndexedSeq[BigInt]] = Future.sequence(xfs)
  val xf: Future[BigInt] = for (ls <- xsf) yield sum(ls.to(LazyList))
  xf foreach println
  private val expected: Future[BigInt] = xf filter (_ == BigInt(chunk * 10 * (chunk * 10 + 1L) / 2))
  expected foreach { _ => println("OK") }

  Await.ready(expected, 10000 milli)
  println("Goodbye")
}
