/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.Scalar
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 10/19/16.
  */
//noinspection ScalaStyle
class SQLParserSpec extends FlatSpec with Matchers {

  private val parser = new SQLParser
  private val w0 = "SELECT * FROM t"
  private val w1 = "SELECT a FROM t"
  private val w2 = "SELECT a AS b FROM t"
  private val w3 = "SELECT a AS b FROM t WHERE d = e"
  private val w4 = "SELECT * FROM t LIMIT 100"
  private val w5 = "SELECT a, CASE WHEN TRUE THEN 42 END FROM t"
  private val w6 = "SELECT a, CASE WHEN TRUE THEN 42 END AS fortytwo FROM t"
  private val w7 = "SELECT a AS b FROM t WHERE d = e AND x = y"
  private val w8 = "SELECT a FROM t ORDER BY a"

  behavior of "select"

  it should "parse " + w0 in {
    val expected = InvocationSelect(List[Invocation](), Scalar("t"), None, None, None)
    val r: parser.ParseResult[Invocation] = parser.parseAll(parser.select, w0)
    r should matchPattern { case parser.Success(`expected`, _) => }
    println(r.get.render())
  }

  it should "parse " + w1 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left("a")), None)), "t", None, None, None)
    val r = parser.parseAll(parser.select, w1)
    r should matchPattern { case parser.Success(`expected`, _) => }
    println(r.get.render())
  }

  it should "parse " + w2 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left("a")), Some("b"))), "t", None, None, None)
    val r = parser.parseAll(parser.select, w2)
    r should matchPattern { case parser.Success(`expected`, _) => }
    println(r.get.render())
  }

  it should "parse " + w3 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left(Scalar("a"))), Some(Scalar("b")))), Scalar("t"), Some(InvocationBooleanExpression(Right(InvocationComparison(Right(InvocationLookup("d")), "=", Right(InvocationLookup("e")))), List())), None, None)
    parser.parseAll(parser.select, w3) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w4 in {
    val expected = InvocationSelect(List[Invocation](), "t", None, Some(100), None)
    parser.parseAll(parser.select, w4) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w5 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left("a")), None), InvocationColumn(InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Left(true), List()), Left(42))), None), None)), "t", None, None, None)
    parser.parseAll(parser.select, w5) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w6 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left("a")), None), InvocationColumn(InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Left(true), List()), Left(42))), None), Some("fortytwo"))), "t", None, None, None)
    parser.parseAll(parser.select, w6) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w7 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left(Scalar("a"))), Some(Scalar("b")))), Scalar("t"), Some(InvocationBooleanExpression(Right(InvocationComparison(Right(InvocationLookup("d")), "=", Right(InvocationLookup("e")))), List(BooleanTerm(And, Right(InvocationComparison(Right(InvocationLookup("x")), "=", Right(InvocationLookup("y")))))))), None, None)
    parser.parseAll(parser.select, w7) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w8 in {
    val expected = InvocationSelect(List(InvocationColumn(InvocationP(Left("a")), None)), "t", None, None, Some(Scalar("a")))
    parser.parseAll(parser.select, w8) should matchPattern { case parser.Success(`expected`, _) => }
  }

}
