package edu.neu.coe.csye7200

object PrimeNumber extends App {
  def primeStream(s: Stream[Int]): Stream[Int] =
    Stream.cons(s.head, primeStream(s.tail filter {
      _ % s.head != 0
    }))

  val primes = primeStream(Stream.from(2))
  println("type in a integer:")
  val num = scala.io.StdIn.readInt()
  val s: Seq[Int] = primes take num
  println(s"the $num th prime number is:")
  println(s.reverse.head)
}
