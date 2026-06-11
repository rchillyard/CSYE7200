package edu.neu.coe.csye7200.fp.ast

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecisionTreeSpec extends AnyFlatSpec with Matchers {

  behavior of "render"
  it should "work" in {
    val tree = DecisionTree(Seq("a", "b", "c", "d"))
    tree("", "") shouldBe "  if (a > b && a > c && a > d)   if (b > c && b > d) if (c>d) \"dcba\" else \"cdba\"\n  else  if (c > b && c > d) if (b>d) \"dbca\" else \"bdca\"\n  else   if (b>c) \"cbda\" else \"bcda\"\n \n  else  if (b > a && b > c && b > d)   if (a > c && a > d) if (c>d) \"dcab\" else \"cdab\"\n  else  if (c > a && c > d) if (a>d) \"dacb\" else \"adcb\"\n  else   if (a>c) \"cadb\" else \"acdb\"\n \n  else  if (c > a && c > b && c > d)   if (a > b && a > d) if (b>d) \"dbac\" else \"bdac\"\n  else  if (b > a && b > d) if (a>d) \"dabc\" else \"adbc\"\n  else   if (a>b) \"badc\" else \"abdc\"\n \n  else     if (a > b && a > c) if (b>c) \"cbad\" else \"bcad\"\n  else  if (b > a && b > c) if (a>c) \"cabd\" else \"acbd\"\n  else   if (a>b) \"bacd\" else \"abcd\"\n \n "
  }

}
