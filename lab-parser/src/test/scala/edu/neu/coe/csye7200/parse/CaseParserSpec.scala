/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.Scalar
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 10/19/16.
  */
//noinspection ScalaStyle,ScalaStyle
class CaseParserSpec extends FlatSpec with Matchers {

  private val parser = new CaseParser
  private val w0 = "WHEN true THEN 42"
  private val w1 = "WHEN (true) THEN 42"
  private val w2 = "WHEN (true) THEN (42)"
  private val w3 = "WHEN true THEN (42)"
  private val c1 = "CASE WHEN (true) THEN (42) ELSE (99) END"
  private val c2 = "CASE WHEN (true) THEN 42 ELSE 99 END"
  private val c3 = "CASE WHEN (FALSE) THEN 0 WHEN (FALSE) THEN SLR.ACCOUNT ELSE SLR.PRODUCT END"
  private val c4 = "CASE WHEN (FALSE) THEN 0 WHEN TRUE THEN SLR.ACCOUNT END"

  behavior of "whenThen"

  it should "parse " + w0 in {
    val expected = InvocationWhenThen(InvocationBooleanExpression(Left(true), List()), Left(42))
    parser.parseAll(parser.whenThen, w0) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w1 in {
    val expected = InvocationWhenThen(InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(true), List())), List()), Left(42))
    parser.parseAll(parser.whenThen, w1) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w2 in {
    val expected = InvocationWhenThen(InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(true), List())), List()), Right(InvocationP(Left(42))))
    parser.parseAll(parser.whenThen, w2) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + w3 in {
    val expected = InvocationWhenThen(InvocationBooleanExpression(Left(true), List()), Right(InvocationP(Left(42))))
    parser.parseAll(parser.whenThen, w3) should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "case"

  it should "parse " + c1 in {
    val expected = InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(true), List())), List()), Right(InvocationP(Left(42))))), Some(Right(InvocationP(Left(99)))))
    parser.parseAll(parser.caseClause, c1) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + c2 in {
    val expected = InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(true), List())), List()), Left(42))), Some(Left(99)))
    parser.parseAll(parser.caseClause, c2) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + c3 in {
    val expected = InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(false), List())), List()), Left(0)), InvocationWhenThen(InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(false), List())), List()), Right(InvocationLookup("SLR.ACCOUNT")))), Some(Right(InvocationLookup("SLR.PRODUCT"))))
    parser.parseAll(parser.caseClause, c3) should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should "parse " + c4 in {
    val expected = InvocationCaseClause(List(
      InvocationWhenThen(
        InvocationBooleanExpression(Right(InvocationBooleanExpression(Left(false), List())), List()),
        Left(0)
      ),
      InvocationWhenThen(
        InvocationBooleanExpression(Left(true), List()),
        Right(InvocationLookup("SLR.ACCOUNT"))
      )
    ), None)
    parser.parseAll(parser.caseClause, c4) should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "parseCaseClause"

  behavior of "opCompare"
  it should "parse all operators" in {
    val (gt, ge, eq, le, lt, ne1, ne2) = (">", ">=", "=", "<=", "<", "<>", "!=")
    parser.parseAll(parser.opCompare, gt) should matchPattern { case parser.Success(`gt`, _) => }
    parser.parseAll(parser.opCompare, ge) should matchPattern { case parser.Success(`ge`, _) => }
    parser.parseAll(parser.opCompare, eq) should matchPattern { case parser.Success(`eq`, _) => }
    parser.parseAll(parser.opCompare, le) should matchPattern { case parser.Success(`le`, _) => }
    parser.parseAll(parser.opCompare, lt) should matchPattern { case parser.Success(`lt`, _) => }
    parser.parseAll(parser.opCompare, ne1) should matchPattern { case parser.Success(`ne1`, _) => }
    parser.parseAll(parser.opCompare, ne2) should matchPattern { case parser.Success(`ne2`, _) => }
  }

  behavior of "comparison"
  private val lookupX = Right(InvocationLookup("x"))
  private val lookupY = Right(InvocationLookup("y"))
  it should "parse a comparison" in {
    val expected1 = InvocationComparison(lookupX, "=", lookupY)
    parser.parseAll(parser.comparison, "x = y") should matchPattern { case parser.Success(`expected1`, _) => }
    val expected2 = InvocationComparison(lookupX, ">", lookupY)
    parser.parseAll(parser.comparison, "x > y") should matchPattern { case parser.Success(`expected2`, _) => }
    val expected3 = InvocationComparison(lookupX, ">=", lookupY)
    parser.parseAll(parser.comparison, "x >= y") should matchPattern { case parser.Success(`expected3`, _) => }
    val expected4 = InvocationComparison(lookupX, "<=", lookupY)
    parser.parseAll(parser.comparison, "x <= y") should matchPattern { case parser.Success(`expected4`, _) => }
    val expected5 = InvocationComparison(lookupX, "<", lookupY)
    parser.parseAll(parser.comparison, "x < y") should matchPattern { case parser.Success(`expected5`, _) => }
    val expected6 = InvocationComparison(lookupX, "<>", lookupY)
    parser.parseAll(parser.comparison, "x <> y") should matchPattern { case parser.Success(`expected6`, _) => }
  }

  it should "parse between" in {
    val expected1 = InvocationBooleanExpression(Right(InvocationComparison(lookupY, "<=", lookupX)), List(BooleanTerm(And, Right(InvocationComparison(lookupX, "<=", Right(InvocationLookup("z")))))))
    parser.parseAll(parser.range, "x between y and z") should matchPattern { case parser.Success(`expected1`, _) => }
  }

  behavior of "predicate"
  it should "parse a comparison" in {
    val expected1 = Right(InvocationComparison(lookupX, "=", lookupY))
    parser.parseAll(parser.predicate, "x = y") should matchPattern { case parser.Success(`expected1`, _) => }
  }
  it should "parse a function" in {
    val expected1 = Right(InvocationFP("NOT", Left(true)))
    parser.parseAll(parser.predicate, "NOT true") should matchPattern { case parser.Success(`expected1`, _) => }
  }

  behavior of "compareFunction"
  it should "parse a comparison" in {
    val expected1 = InvocationBooleanExpression(Right(InvocationComparison(lookupX, "=", lookupY)), List())
    parser.parseAll(parser.compareFunction, "(x = y)") should matchPattern { case parser.Success(`expected1`, _) => }
  }
  it should "parse an expression" in {
    val expected1 = InvocationBooleanExpression(Right(InvocationComparison(lookupX, "=", lookupY)), List(BooleanTerm(And, Right(InvocationComparison(lookupY, "=", lookupX)))))
    parser.parseAll(parser.compareFunction, "(x = y and y = x)") should matchPattern { case parser.Success(`expected1`, _) => }
  }

  behavior of "booleanTerm"
  it should "parse AND+comparison" in {
    val expected1 = BooleanTerm(And, Right(InvocationComparison(lookupX, "=", lookupY)))
    parser.parseAll(parser.booleanTerm, "AND x = y") should matchPattern { case parser.Success(`expected1`, _) => }
  }
  it should "parse a OR+function" in {
    val expected1 = BooleanTerm(Or, Right(InvocationFP("NOT", Left(true))))
    parser.parseAll(parser.booleanTerm, "OR NOT true") should matchPattern { case parser.Success(`expected1`, _) => }
  }

  behavior of "booleanExpression"
  it should "parse comparison+AND+comparison" in {
    val expected1 = InvocationBooleanExpression(Right(InvocationComparison(lookupY, "=", lookupX)), List(BooleanTerm(And, Right(InvocationComparison(lookupX, "=", lookupY)))))
    parser.parseAll(parser.booleanExpression, "y = x and x = y") should matchPattern { case parser.Success(`expected1`, _) => }
  }
  it should "parse a comparison+OR+function" in {
    val expected0 = BooleanTerm(Or, Right(InvocationFP("NOT", Left(true))))
    val expected1 = InvocationBooleanExpression(Right(InvocationBooleanExpression(Right(InvocationComparison(lookupX, "=", lookupY)), List())), List(expected0))
    parser.parseAll(parser.booleanExpression, "(x = y) OR NOT true") should matchPattern { case parser.Success(`expected1`, _) => }
  }
  it should "parse complex boolean expression" in {
    val expected1 = InvocationBooleanExpression(Right(InvocationComparison(Right(InvocationLookup("a")), "=", Right(InvocationLookup("b")))), List(BooleanTerm(And, Right(InvocationBooleanExpression(Right(InvocationComparison(Right(InvocationLookup("c")), ">", Right(InvocationLookup("d")))), List(BooleanTerm(Or, Right(InvocationComparison(lookupX, "=", lookupY)))))))))
    parser.parseAll(parser.booleanExpression, "a = b AND (c > d OR x = y)") should matchPattern { case parser.Success(`expected1`, _) => }
  }

  behavior of "between"
  it should "parse between variables" in {
    val expected1 = InvocationBooleanExpression(Right(InvocationComparison(Right(InvocationLookup("a")), "<=", Right(InvocationLookup("b")))), List(BooleanTerm(And, Right(InvocationComparison(Right(InvocationLookup("b")), "<=", Right(InvocationLookup("c")))))))
    parser.parseAll(parser.range, "b between a AND c") should matchPattern { case parser.Success(`expected1`, _) => }
  }
  it should "parse between constants" in {
    val expected1 = Right(InvocationBooleanExpression(Right(InvocationComparison(Left(Scalar(1)), "<=", Right(InvocationLookup("b")))), List(BooleanTerm(And, Right(InvocationComparison(Right(InvocationLookup("b")), "<=", Left(Scalar(3))))))))
    parser.parseAll(parser.predicate, "b between 1 AND 3") should matchPattern { case parser.Success(`expected1`, _) => }
  }
}
