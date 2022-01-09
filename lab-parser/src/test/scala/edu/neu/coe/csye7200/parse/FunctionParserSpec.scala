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

  private val p = FunctionParser

  private def qss(s: String) = QuotedStringScalar(s)

  private def ls(s: Scalar): Expression = Left(s)

  private def lqss(s: String): Expression = ls(qss(s))

  behavior of "variable"
  it should """parse correctly""" in {
    p.parseAll(p.variable, "X") should matchPattern { case p.Success(InvocationLookup("X"), _) => }
    p.parseAll(p.variable, "LIKE") should matchPattern { case p.Failure(_, _) => }
  }

  behavior of "number"
  it should """yield 42 from "42"""" in {
    p.parseAll(p.number, "42") should matchPattern { case p.Success(42, _) => }
  }

  behavior of "boolean"
  it should """yield true from "TRUE"""" in {
    p.parseAll(p.boolean, "TRUE") should matchPattern { case p.Success(true, _) => }
  }

  behavior of "date"
  it should """parse date in ISO sequence""" in {
    p.parseAll(p.isoDate, "2015-09-30") should matchPattern { case p.Success("2015-09-30", _) => }
    p.parseAll(p.isoDate, "2015-sep-30") should matchPattern { case p.Success("2015-sep-30", _) => }
    p.parseAll(p.isoDate, "2015-SEP-30") should matchPattern { case p.Success("2015-SEP-30", _) => }
    p.parseAll(p.date, "2015-09-30") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
    p.parseAll(p.date, "2015-sep-30") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
    p.parseAll(p.date, "2015-SEP-30") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
  }

  it should """parse date in UK sequence""" in {
    p.parseAll(p.ukTradDate, "30-sep-2015") should matchPattern { case p.Success("2015-sep-30", _) => }
    p.parseAll(p.date, "30-sep-2015") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
  }

  it should """parse date in US sequence""" in {
    p.parseAll(p.usDate, "09-30-2015") should matchPattern { case p.Success("2015-09-30", _) => }
    p.parseAll(p.usDate, "09/30/2015") should matchPattern { case p.Success("2015-09-30", _) => }
    p.parseAll(p.usDate, "09-30-15") should matchPattern { case p.Success("15-09-30", _) => }
    p.parseAll(p.date, "09/30/2015") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
    p.parseAll(p.date, "sep-30-2015") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
    p.parseAll(p.date, "09/30/15") should matchPattern { case p.Success(IsoDate(2015, 9, 30), _) => }
  }

  behavior of "pn1Identifier"
  it should """parse concat and IN correctly""" in {
    p.parseAll(p.pn1Identifier, "concat") should matchPattern { case p.Success("concat", _) => }
    p.parseAll(p.pn1Identifier, "IN") should matchPattern { case p.Success("IN", _) => }
    p.parseAll(p.pn1Identifier, "in") should matchPattern { case p.Success("in", _) => }
  }

  behavior of "fpIdentifier"
  it should """parse NOT and LIKE correctly""" in {
    p.parseAll(p.fpIdentifier, "NOT") should matchPattern { case p.Success("NOT", _) => }
    p.parseAll(p.fpIdentifier, "LIKE") should matchPattern { case p.Success("LIKE", _) => }
  }

  behavior of "term"
  it should """yield the correct values""" in {
    p.parseAll(p.term, "42") should matchPattern { case p.Success(Scalar(42), _) => }
    p.parseAll(p.term, "TRUE") should matchPattern { case p.Success(Scalar(true), _) => }
    p.parseAll(p.term, "'HELLO'") should matchPattern { case p.Success(QuotedStringScalar("HELLO", _), _) => }
    p.parseAll(p.term, "HELLO") should matchPattern { case p.Failure(_, _) => }
    p.parseAll(p.term, "concat") should matchPattern { case p.Failure(_, _) => }
  }

  it should """yield date from '20150930'""" in {
    val expected: DateScalar = DateScalar(2015, 9, 30)
    p.parseAll(p.term, "'20150930'") should matchPattern { case p.Success(`expected`, _) => }
  }

  it should """yield date from "2015-09-30"""" in {
    val expected: DateScalar = DateScalar(2015, 9, 30)
    p.parseAll(p.term, "2015-09-30") should matchPattern { case p.Success(`expected`, _) => }
  }


  behavior of "parameter"
  it should """yield the correct values""" in {
    p.parseAll(p.parameter, "42") should matchPattern { case p.Success(Left(Scalar(42)), _) => }
    p.parseAll(p.parameter, "TRUE") should matchPattern { case p.Success(Left(Scalar(true)), _) => }
    p.parseAll(p.parameter, "'HELLO'") should matchPattern { case p.Success(Left(QuotedStringScalar("HELLO", _)), _) => }
    p.parseAll(p.parameter, "HELLO") should matchPattern { case p.Success(Right(InvocationLookup("HELLO")), _) => }
    p.parseAll(p.parameter, "NOT TRUE") should matchPattern { case p.Success(Right(InvocationBase("NOT", List(Left(Scalar(true))))), _) => }
  }

  behavior of "pn1"
  it should """yield InvocationFPn for concat""" in {
    val expected = InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))
    p.parseAll(p.pn1, "concat(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn for nested call with IN""" in {
    val expected = InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR"))))))
    p.parseAll(p.pn1, "IN ('UK','US','FR')") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn for nested call with in""" in {
    val expected = InvocationFPn("in", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR"))))))
    p.parseAll(p.pn1, "in ('UK','US','FR')") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "fPn"
  it should """yield InvocationFPn for x(SLR.COUNTRY_OF_INCORPORATION)""" in {
    val expected = InvocationFPn("x", List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION"))))
    p.parseAll(p.fPn, "x(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn for nested call""" in {
    //    val expected = InvocationFPn("getValStdJoin",List(Right(InvocationFPn("concat",List(Right(InvocationPn1(List(lss("SLR.COUNTRY_OF_INCORPORATION"))))))), lss("SLR.REPORTING_PERIOD_RUN_CONTROL"), lqss("GCRA_FTP_COUNTRY_TO_CUSTOM"), Left("GLBL_FILLER_10")))
    val expected = InvocationFPn("getValStdJoin", List(Right(InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))), Right(InvocationLookup("SLR.REPORTING_PERIOD_RUN_CONTROL")), lqss("GCRA_FTP_COUNTRY_TO_CUSTOM"), lqss("GLBL_FILLER_10")))
    p.parseAll(p.fPn, "getValStdJoin(concat(SLR.COUNTRY_OF_INCORPORATION), SLR.REPORTING_PERIOD_RUN_CONTROL, 'GCRA_FTP_COUNTRY_TO_CUSTOM','GLBL_FILLER_10')") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "pf"
  it should """yield InvocationPF""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))
    p.parseAll(p.pf, "SLR.COUNTRY_OF_INCORPORATION IN ('UK','US','FR')") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationFP for LIKE""" in {
    val expected = InvocationPF(lqss("abc"), InvocationFP("LIKE", lqss("a%c")))
    p.parseAll(p.pf, "'abc' LIKE 'a%c'") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "fp"
  it should """yield InvocationFP for NOT""" in {
    val expected = InvocationFP("NOT", ls(Scalar(true)))
    p.parseAll(p.fp, "NOT TRUE") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "p"
  it should """parse (x)""" in {
    val expected = InvocationP(Right(InvocationLookup("x")))
    p.parseAll(p.p, "(x)") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "parameterSet"
  it should """parse a single-term parameter set""" in {
    val expected1 = List(lqss("X"))
    p.parseAll(p.parameterSet, "('X')") should matchPattern { case p.Success(`expected1`, _) => }
    val expected2 = List(Right(InvocationLookup("X")))
    p.parseAll(p.parameterSet, "(X)") should matchPattern { case p.Success(`expected2`, _) => }
    val expected3 = List(Right(InvocationLookup("X.Y")))
    p.parseAll(p.parameterSet, "(X.Y)") should matchPattern { case p.Success(`expected3`, _) => }
  }

  it should """parse a triple-term parameter set""" in {
    val expected = List(lqss("X"), lqss("Y"), lqss("Z"))
    p.parseAll(p.parameterSet, "('X','Y','Z')") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "function"
  it should """not parse X as a function""" in {
    p.parseAll(p.function, "X") should matchPattern { case p.Failure(_, _) => }
  }
  it should """parse (X) as a function""" in {
    val expected = InvocationP(Right(InvocationLookup("X")))
    p.parseAll(p.function, "(X)") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationFPn/InvocationPn1/InvocationLookup for concat""" in {
    val expected = InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))
    p.parseAll(p.function, "concat(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationPF/InvocationFPn/InvocationPn1""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))
    p.parseAll(p.function, "SLR.COUNTRY_OF_INCORPORATION IN ('UK','US','FR')") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationFP""" in {
    val expected = InvocationFP("NOT", ls(Scalar(true)))
    p.parseAll(p.function, "NOT TRUE") should matchPattern { case p.Success(`expected`, _) => }
  }
  it should """yield InvocationPF/InvocationFP""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFP("NOT", Right(InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))))
    p.parseAll(p.function, "SLR.COUNTRY_OF_INCORPORATION NOT IN ('UK','US','FR')") should matchPattern { case p.Success(`expected`, _) => }
  }

  behavior of "parseFunctionCall"
  it should """yield InvocationFPn for concat""" in {
    val expected = InvocationFPn("concat", List(Right(InvocationPn1(List(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")))))))
    p.parseFunctionCall("concat(SLR.COUNTRY_OF_INCORPORATION)") should matchPattern { case Success(`expected`) => }
  }
  it should """yield InvocationPF""" in {
    val expected = InvocationPF(Right(InvocationLookup("SLR.COUNTRY_OF_INCORPORATION")), InvocationFPn("IN", List(Right(InvocationPn1(List(lqss("UK"), lqss("US"), lqss("FR")))))))
    p.parseFunctionCall("SLR.COUNTRY_OF_INCORPORATION IN ('UK','US','FR')") should matchPattern { case Success(`expected`) => }
  }
  it should """yield InvocationFP""" in {
    val expected = InvocationFP("NOT", ls(Scalar(true)))
    p.parseFunctionCall("NOT TRUE") should matchPattern { case Success(`expected`) => }
  }

}
