package edu.neu.coe.csye7200.fp.patternmatching

import org.scalatest.funsuite.AnyFunSuite

/**
  * Created by scalaprof on 9/10/16.
  */
//noinspection NameBooleanParameters
class Document$Test extends AnyFunSuite {

  test("singleton Document") {
    val t1: Document[String, Int] = SingletonDocument(42)
    assert(t1.size == 1)
    assert(t1.get(List()).contains(42))
    assert(t1.get(List("a")).isEmpty)
    assert(t1.toString == "42")
    assert(t1.asInstanceOf[SingletonDocument[String, Int]].convertToMapDocument(null.asInstanceOf[String]) == MapDocument(Map(null.asInstanceOf[String] -> t1)))
  }

  test("map Document") {
    val t1: Document[String, Int] = SingletonDocument(42)
    val m1: Document[String, Int] = MapDocument(Map("x" -> t1))
    assert(t1.size == 1)
    assert(m1.size == 1)
    assert(m1.get(List()).isEmpty)
    assert(m1.get(List("x")).contains(42))
    assert(m1.toString == "Map(x -> 42)")
  }

  test("Document ++") {
    val t1: Document[String, Int] = SingletonDocument(42)
    val m1: Document[String, Int] = MapDocument(Map("x" -> t1))
    assert((m1 ++ t1).size == 2)
    assert((t1 ++ m1).size == 2)
    assert(DocumentMain.++(t1, m1).size == 2)
  }

  test("extract SingletonDocument1") {
    SingletonDocument(42) match {
      case SingletonDocument(t) => assert(t == 42)
      case _ => assert(false)
    }
  }
  // TODO not sure what this test for
  ignore("extract SingletonDocument2") {
    SingletonDocument[String, Int](42) :+ ((Nil, SingletonDocument(99))) match {
      case MapDocument(m) => println(m); assert(m == Map("x" -> SingletonDocument(42)))
      case _ => assert(false)
    }
  }

  test("extract MapDocument1") {
    MapDocument(Map("x" -> SingletonDocument[String, Int](42))) match {
      case MapDocument(m) => assert(m == Map("x" -> SingletonDocument(42)))
      case _ => assert(false)
    }
  }
}
