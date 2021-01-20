package edu.neu.coe.csye7200.fp.balancedtree

import org.scalatest.flatspec
import org.scalatest.matchers.should

class NodeSpec extends flatspec.AnyFlatSpec with should.Matchers {

  behavior of "degree"
  it should "work for TwoNode" in {
    val target = TwoNode[String, String]("K", None, None)
    target.degree shouldBe 2
  }
  it should "work for ThreeNode" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.degree shouldBe 3
  }

  behavior of "children"
  it should "work for TwoNode" in {
    val target = TwoNode[String, String]("K", None, None)
    target.children.size shouldBe 2
    target.children.flatMap(_.toSeq).size shouldBe 0
  }
  it should "work for ThreeNode" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.children.size shouldBe 3
    target.children.flatMap(_.toSeq).size shouldBe 0
  }


  behavior of "compare"
  it should "work for TwoNode (key)" in {
    val target = TwoNode[String, String]("K", None, None)
    target.compare("K") should matchPattern { case (0, _) => }
  }
  it should "work for TwoNode (left)" in {
    val target = TwoNode[String, String]("K", None, None)
    target.compare("H") should matchPattern { case (-1, 0) => }
  }
  it should "work for TwoNode (right)" in {
    val target = TwoNode[String, String]("K", None, None)
    target.compare("M") should matchPattern { case (1, 0) => }
  }
  it should "work for ThreeNode (left)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.compare("H") should matchPattern { case (-1, 0) => }
  }
  it should "work for ThreeNode (key1)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.compare("K") should matchPattern { case (0, _) => }
  }
  it should "work for ThreeNode (middle)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.compare("L") should matchPattern { case (-1, 1) => }
  }
  it should "work for ThreeNode (key2)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.compare("M") should matchPattern { case (0, _) => }
  }
  it should "work for ThreeNode (right)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.compare("N") should matchPattern { case (1, 1) => }
  }

  behavior of "find"
  it should "work for TwoNode (left)" in {
    val target = TwoNode[String, String]("K", None, None)
    target.find("H") should matchPattern { case (Left(false), 0) => }
  }
  it should "work for TwoNode (key)" in {
    val target = TwoNode[String, String]("K", None, None)
    target.find("K") should matchPattern { case (Right(`target`), 0) => }
  }
  it should "work for TwoNode (right)" in {
    val target = TwoNode[String, String]("K", None, None)
    target.find("M") should matchPattern { case (Left(true), 0) => }
  }
  it should "work for ThreeNode (left)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.find("H") should matchPattern { case (Left(false), 0) => }
  }
  it should "work for ThreeNode (key1)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.find("K") should matchPattern { case (Right(`target`), 0) => }
  }
  it should "work for ThreeNode (middle)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.find("L") should matchPattern { case (Left(false), 1) => }
  }
  it should "work for ThreeNode (key2)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.find("M") should matchPattern { case (Right(`target`), 1) => }
  }
  it should "work for ThreeNode (right)" in {
    val target = ThreeNode[String, String]("K", "M", None, None, None)
    target.find("N") should matchPattern { case (Left(true), 1) => }
  }

  behavior of "keys"
  it should "work" in {

  }


}
