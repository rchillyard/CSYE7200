/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.{DateScalar, QuotedStringScalar, Scalar}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
  * Created by scalaprof on 10/19/16.
  */
//noinspection ScalaStyle
class FunctionParserSpec extends FlatSpec with Matchers {

  val parser = new FunctionParser

  private def qss(s: String) = QuotedStringScalar(s)

  private def ls(s: Scalar): Expression = Left(s)

  private def lqss(s: String): Expression = ls(qss(s))

  behavior of "variable"
  it should """parse correctly""" in {
    parser.parseAll(parser.variable, "X") should matchPattern { case parser.Success(InvocationLookup("X"), _) => }
    parser.parseAll(parser.variable, "LIKE") should matchPattern { case parser.Failure(_, _) => }
  }

  behavior of "number"
  it should """yield 42 from "42"""" in {
    parser.parseAll(parser.number, "42") should matchPattern { case parser.Success(42, _) => }
  }

  behavior of "boolean"
  it should """yield true from "TRUE"""" in {
    parser.parseAll(parser.boolean, "TRUE") should matchPattern { case parser.Success(true, _) => }
  }

  behavior of "date"
  it should """parse date in ISO sequence""" in {
    parser.parseAll(parser.isoDate, "2015-09-30") should matchPattern { case parser.Success("2015-09-30", _) => }
    parser.parseAll(parser.isoDate, "2015-sep-30") should matchPattern { case parser.Success("2015-sep-30", _) => }
    parser.parseAll(parser.isoDate, "2015-SEP-30") should matchPattern { case parser.Success("2015-SEP-30", _) => }
    parser.parseAll(parser.date, "2015-09-30") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
    parser.parseAll(parser.date, "2015-sep-30") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
    parser.parseAll(parser.date, "2015-SEP-30") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
  }

  it should """parse date in UK sequence""" in {
    parser.parseAll(parser.ukTradDate, "30-sep-2015") should matchPattern { case parser.Success("2015-sep-30", _) => }
    parser.parseAll(parser.date, "30-sep-2015") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
  }

  it should """parse date in US sequence""" in {
    parser.parseAll(parser.usDate, "09-30-2015") should matchPattern { case parser.Success("2015-09-30", _) => }
    parser.parseAll(parser.usDate, "09/30/2015") should matchPattern { case parser.Success("2015-09-30", _) => }
    parser.parseAll(parser.usDate, "09-30-15") should matchPattern { case parser.Success("15-09-30", _) => }
    parser.parseAll(parser.date, "09/30/2015") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
    parser.parseAll(parser.date, "sep-30-2015") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
    parser.parseAll(parser.date, "09/30/15") should matchPattern { case parser.Success(IsoDate(2015, 9, 30), _) => }
  }

  behavior of "pn1Identifier"
  it should """parse concat and IN correctly""" in {
    parser.parseAll(parser.pn1Identifier, "concat") should matchPattern { case parser.Success("concat", _) => }
    parser.parseAll(parser.pn1Identifier, "IN") should matchPattern { case parser.Success("IN", _) => }
    parser.parseAll(parser.pn1Identifier, "in") should matchPattern { case parser.Success("in", _) => }
  }

  behavior of "fpIdentifier"
  it should """parse NOT and LIKE correctly""" in {
    parser.parseAll(parser.fpIdentifier, "NOT") should matchPattern { case parser.Success("NOT", _) => }
    parser.parseAll(parser.fpIdentifier, "LIKE") should matchPattern { case parser.Success("LIKE", _) => }
  }

  behavior of "term"
  it should """yield the correct values""" in {
    parser.parseAll(parser.term, "42") should matchPattern { case parser.Success(Scalar(42), _) => }
    parser.parseAll(parser.term, "TRUE") should matchPattern { case parser.Success(Scalar(true), _) => }
    parser.parseAll(parser.term, "'HELLO'") should matchPattern { case parser.Success(QuotedStringScalar("HELLO", _), _) => }
    parser.parseAll(parser.term, "HELLO") should matchPattern { case parser.Failure(_, _) => }
    parser.parseAll(parser.term, "concat") should matchPattern { case parser.Failure(_, _) => }
  }

  it should """yield date from '20150930'""" in {
    val expected: DateScalar = DateScalar(2015, 9, 30)
    parser.parseAll(parser.term, "'20150930'") should matchPattern { case parser.Success(`expected`, _) => }
  }

  it should """yield date from "2015-09-30"""" in {
    val expected: DateScalar = DateScalar(2015, 9, 30)
    parser.parseAll(parser.term, "2015-09-30") should matchPattern { case parser.Success(`expected`, _) => }
  }


  behavior of "parameter"
  it should """yield the correct values""" in {
    parser.parseAll(parser.parameter, "42") should matchPattern { case parser.Success(Left(Scalar(42)), _) => }
    parser.parseAll(parser.parameter, "TRUE") should matchPattern { case parser.Success(Left(Scalar(true)), _) => }
    parser.parseAll(parser.parameter, "'HELLO'") should matchPattern { case parser.Success(Left(QuotedStringScalar("HELLO", _)), _) => }
    parser.parseAll(parser.parameter, "HELLO") should matchPattern { case parser.Success(Right(InvocationLookup("HELLO")), _) => }
    parser.parseAll(parser.parameter, "NOT TRUE") should matchPattern { case parser.Success(Right(InvocationBase("NOT", List(Left(Scalar(true))))), _) => }
  }

  behavior of "pn1"
  it should """yield InvocationFPn for concat""" in {
    val expected = InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))
    parser.parseAll(parser.pn1, "concat(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn for nested call with IN""" in {
    val expected = InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR"))))))
    parser.parseAll(parser.pn1, "IN ('UK','US','FR')") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn for nested call with in""" in {
    val expected = InvocationFPn("in", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR"))))))
    parser.parseAll(parser.pn1, "in ('UK','US','FR')") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "fPn"
  it should """yield InvocationFPn for x(SLR.COUNTRY_OF_INCORPORATION)""" in {
    val expected = InvocationFPn("x", List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION"))))
    parser.parseAll(parser.fPn, "x(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn for nested call""" in {
    //    val expected = InvocationFPn("getValStdJoin",List(Right(InvocationFPn("concat",List(Right(InvocationPn1(List(lss("SLR.COUNTRY_OF_INCORPORATION"))))))), lss("SLR.REPORTING_PERIOD_RUN_CONTROL"), lqss("GCRA_FTP_COUNTRY_TO_CUSTOM"), Left("GLBL_FILLER_10")))
    val expected = InvocationFPn("getValStdJoin", List(Right(InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))), Right(InvocationLookup("SLR.REPORTING_PERIOD_RUN_CONTROL")), lqss("GCRA_FTP_COUNTRY_TO_CUSTOM"), lqss("GLBL_FILLER_10")))
    parser.parseAll(parser.fPn, "getValStdJoin(concat(SLR.COUNTRY_OF_INCORPORATION), SLR.REPORTING_PERIOD_RUN_CONTROL, 'GCRA_FTP_COUNTRY_TO_CUSTOM','GLBL_FILLER_10')") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "pf"
  it should """yield InvocationPF""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))
    parser.parseAll(parser.pf, "SLR.COUNTRY_OF_INCORPORATION IN ('UK','US','FR')") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationFP for LIKE""" in {
    val expected = InvocationPF(lqss("abc"), InvocationFP("LIKE", lqss("a%c")))
    parser.parseAll(parser.pf, "'abc' LIKE 'a%c'") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "fp"
  it should """yield InvocationFP for NOT""" in {
    val expected = InvocationFP("NOT", ls(Scalar(true)))
    parser.parseAll(parser.fp, "NOT TRUE") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "p"
  it should """parse (x)""" in {
    val expected = InvocationP(Right(InvocationLookup("x")))
    parser.parseAll(parser.p, "(x)") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "parameterSet"
  it should """parse a single-term parameter set""" in {
    val expected1 = List(lqss("X"))
    parser.parseAll(parser.parameterSet, "('X')") should matchPattern { case parser.Success(`expected1`, _) => }
    val expected2 = List(Right(InvocationLookup("X")))
    parser.parseAll(parser.parameterSet, "(X)") should matchPattern { case parser.Success(`expected2`, _) => }
    val expected3 = List(Right(InvocationLookup("X.Y")))
    parser.parseAll(parser.parameterSet, "(X.Y)") should matchPattern { case parser.Success(`expected3`, _) => }
  }

  it should """parse a triple-term parameter set""" in {
    val expected = List(lqss("X"), lqss("Y"), lqss("Z"))
    parser.parseAll(parser.parameterSet, "('X','Y','Z')") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "function"
  it should """not parse X as a function""" in {
    parser.parseAll(parser.function, "X") should matchPattern { case parser.Failure(_, _) => }
  }
  it should """parse (X) as a function""" in {
    val expected = InvocationP(Right(InvocationLookup("X")))
    parser.parseAll(parser.function, "(X)") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn/InvocationPn1/InvocationLookup for concat""" in {
    val expected = InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))
    parser.parseAll(parser.function, "concat(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationPF/InvocationFPn/InvocationPn1""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))
    parser.parseAll(parser.function, "SLR.COUNTRY_OF_INCORPORATION IN ('UK','US','FR')") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationFP""" in {
    val expected = InvocationFP("NOT", ls(Scalar(true)))
    parser.parseAll(parser.function, "NOT TRUE") should matchPattern { case parser.Success(`expected`, _) => }
  }
  it should """yield InvocationPF/InvocationFP""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFP("NOT", Right(InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))))
    parser.parseAll(parser.function, "SLR.COUNTRY_OF_INCORPORATION NOT IN ('UK','US','FR')") should matchPattern { case parser.Success(`expected`, _) => }
  }

  behavior of "parseFunctionCall"
  it should """yield InvocationFPn for concat""" in {
    val expected = InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))
    parser.parseFunctionCall("concat(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case Success(`expected`) => }
  }
  it should """yield InvocationPF""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))
    parser.parseFunctionCall("SLR.COUNTRY_OF_INCORPORATION IN ('UK','US','FR')") should matchPattern { case Success(`expected`) => }
  }
  it should """yield InvocationFP""" in {
    val expected = InvocationFP("NOT", ls(Scalar(true)))
    parser.parseFunctionCall("NOT TRUE") should matchPattern { case Success(`expected`) => }
  }

}
