package edu.neu.coe.csye7200

object Prime1 extends App {
  def buildPrime(k: Int): LazyList[Int] = k match {
    case i if i <= 0 => throw new NoSuchElementException(s"No such element for $k")
    case _ =>
      2 #:: LazyList.from(3).filter(x => {
        !(2 until x).exists(x % _ == 0)
      }).take(k - 1)
  }

  val i = 10
  buildPrime(i).foreach(println(_))
}
