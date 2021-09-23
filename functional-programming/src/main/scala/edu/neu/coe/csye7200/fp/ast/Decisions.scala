package edu.neu.coe.csye7200.fp.ast

case class Decisions(a: Int, b: Int, c: Int, d: Int) extends (() => String) {

  override def apply(): String =

    if (a > b && a > c && a > d) if (b > c && b > d) if (c > d) "dcba" else "cdba"
    else if (c > b && c > d) if (b > d) "dbca" else "bdca"
    else if (b > c) "cbda" else "bcda"

    else if (b > a && b > c && b > d) if (a > c && a > d) if (c > d) "dcab" else "cdab"
    else if (c > a && c > d) if (a > d) "dacb" else "adcb"
    else if (a > c) "cadb" else "acdb"

    else if (c > a && c > b && c > d) if (a > b && a > d) if (b > d) "dbac" else "bdac"
    else if (b > a && b > d) if (a > d) "dabc" else "adbc"
    else if (a > b) "badc" else "abdc"

    else if (a > b && a > c) if (b > c) "cbad" else "bcad"
    else if (b > a && b > c) if (a > c) "cabd" else "acbd"
    else if (a > b) "bacd" else "abcd"

}

case class Decisions2(a: Int, b: Int, c: Int, d: Int) extends (() => String) {

  override def apply(): String =
    if (a > b && a > c && a > d) if (b > c && b > d) if (c > d) "dcba" else "cdba"
    else if (c > b && c > d) if (b > d) "dbca" else "bdca"
    else if (b > c) "cbda" else "bcda"

    else if (b > a && b > c && b > d) if (a > c && a > d) if (c > d) "dcab" else "cdab"
    else if (c > a && c > d) if (a > d) "dacb" else "adcb"
    else if (a > c) "cadb" else "acdb"

    else if (c > a && c > b && c > d) if (a > b && a > d) if (b > d) "dbac" else "bdac"
    else if (b > a && b > d) if (a > d) "dabc" else "adbc"
    else if (a > b) "badc" else "abdc"

    else if (a > b && a > c) if (b > c) "cbad" else "bcad"
    else if (b > a && b > c) if (a > c) "cabd" else "acbd"
    else if (a > b) "bacd" else "abcd"

}

object Decisions extends App {
  val d1 = Decisions(1, 3, 2, 5)
  println(d1.apply())
}
