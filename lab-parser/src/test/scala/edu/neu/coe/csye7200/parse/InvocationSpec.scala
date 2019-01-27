/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.{QuotedStringScalar, Scalar}
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by scalaprof on 10/19/16.
  */
//noinspection ScalaStyle
class InvocationSpec extends FlatSpec with Matchers {

  def mapLookup[X]: Map[String, X] => String => Option[X] = { m => s => m.get(s) }

  behavior of "InvocationBase"
  //  it should "invert function of two parameters" in {
  //    def mapLookup[X]: Map[String, X] => String => Option[X] = { m => s => m.get(s) }
  //
  //    val rowData = Map("SLR.ACCOUNT" -> "x10177789", "k" -> "K")
  //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
  //    implicit val functionLibrary = FunctionLibrary.create(lookup)
  //
  //    val inversion = Inversion(0, 2)
  //
  //    def fAttrTableLookup(p1: Boolean, p2: String): Boolean = p1
  //
  //    val paramsAttrTableLookup = List("ifFound", "sjKey")
  //    val convertersAttrTableLookup = List(s2b, s2s)
  //    val gfAttrTableLookup = RenderableFunction(fAttrTableLookup _, FunctionString.custom("attribTableLookup", paramsAttrTableLookup), RenderableFunction.callByValue(2))
  //    val ifAttrTableLookup = InvocationFunction(gfAttrTableLookup, None, convertersAttrTableLookup)
  //    val ifAttrTableLookup$ = ifAttrTableLookup.invert(inversion)
  //    val fy: Try[InvocationFunction[Boolean]] = ifAttrTableLookup$.applyExpressions[String](List(lqss("k")))
  //    val by: Try[Boolean] = for (f1 <- fy;
  //                                scalar: Scalar = Scalar(true);
  //                                f2 <- f1.applyExpressions[String](List(Left(scalar)));
  //                                b <- f2.evaluate) yield b
  //    by should matchPattern { case Success(true) => }
  //  }

  behavior of "InvocationP"
  //  it should "work" in {
  //    def fIdentity(p: Any): Any = p
  //
  //    val gIdentity = RenderableFunction(fIdentity _, "identity", Seq(false))
  //    val ifIdentity = InvocationFunction(gIdentity, None, List(s2s))
  //    val rowData = Map("k" -> "K")
  //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
  //    implicit val functionLibrary = FunctionLibrary.create(lookup) + ("identity" -> ifIdentity)
  //    val invocation = InvocationP(Right(InvocationLookup("k")))
  //    val wy: Try[String] = for (c1 <- invocation.asClosure[String, String]; c2 <- c1.asClosure[String]; b <- c2()) yield b
  //    wy should matchPattern { case Success("K") => }
  //  }

  behavior of "InvocationFPn"
  //  it should """evaluate to true with recHierarchyLookUp(...)""" in {
  //    def fRecHierarchyLookup(p1: Boolean, p2: Boolean, p3: String, p4: String, p5: String, p6: String, p7: String): Boolean = true
  //
  //    val paramsRecHierarchyLookup = List("ifFound", "isNode", "setId", "treeName", "nodeString", "nodeDetail", "eff")
  //    val convertersRecHierarchyLookup = List(s2b, s2b, s2s, s2s, s2s, s2s, s2s)
  //    val gfRecHierarchyLookup = RenderableFunction(fRecHierarchyLookup _, FunctionString.custom("recHierarchyLookup", paramsRecHierarchyLookup), RenderableFunction.callByValue(7))
  //    val ifRecHierarchyLookup = InvocationFunction(gfRecHierarchyLookup, None, convertersRecHierarchyLookup)
  //
  //    val rowData = Map("k" -> "K")
  //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
  //
  //    implicit val functionLibrary = FunctionLibrary.create(lookup) + ("rechierarchylookup" -> ifRecHierarchyLookup)
  //    val variables = List(Left(BooleanScalar("TRUE")), Left(BooleanScalar("FALSE")), Left(qss("GLOBL")), Left(qss("GLBR_0040_ACC_TB")), Left(qss("4112100000~4112200000~4112300000~4112400000~4112500000")), Left(qss("x10177789")), Left(qss("20161123")))
  //    val invocation = InvocationFPn("recHierarchyLookUp", variables)
  //    val by: Try[Boolean] = for (c1 <- invocation.asClosure[String, Boolean]; c2 <- c1.asClosure[String]; b <- c2()) yield b
  //    by should matchPattern { case Success(true) => }
  //  }
  //
  //  it should """evaluate to true with recHierarchyLookUp(...) and "SLR.ACCOUNT" -> "x10177789" """ in {
  //    def fRecHierarchyLookup(p1: Boolean, p2: Boolean, p3: String, p4: String, p5: String, p6: String, p7: String): Boolean = true
  //
  //    val paramsRecHierarchyLookup = List("ifFound", "isNode", "setId", "treeName", "nodeString", "nodeDetail", "eff")
  //    val convertersRecHierarchyLookup = List(s2b, s2b, s2s, s2s, s2s, s2s, s2s)
  //    val gfRecHierarchyLookup = RenderableFunction(fRecHierarchyLookup _, FunctionString.custom("recHierarchyLookup", paramsRecHierarchyLookup), RenderableFunction.callByValue(7))
  //    val ifRecHierarchyLookup = InvocationFunction(gfRecHierarchyLookup, None, convertersRecHierarchyLookup)
  //
  //    val rowData = Map("SLR.ACCOUNT" -> "x10177789", "k" -> "K")
  //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
  //
  //    implicit val functionLibrary = FunctionLibrary.create(lookup) + ("rechierarchylookup" -> ifRecHierarchyLookup)
  //    val variables = List(Left(BooleanScalar("TRUE")), Left(BooleanScalar("FALSE")), Left(qss("GLOBL")), Left(qss("GLBR_0040_ACC_TB")), Left(qss("4112100000~4112200000~4112300000~4112400000~4112500000")), Left(StringScalar("SLR.ACCOUNT")), Left(qss("20161123")))
  //    val invocation = InvocationFPn("recHierarchyLookUp", variables)
  //    val by: Try[Boolean] = for (c1 <- invocation.asClosure[Any, Boolean]; c2 <- c1.asClosure[String]; b <- c2()) yield b
  //    by should matchPattern { case Success(true) => }
  //  }

  behavior of "InvocationPn1"
  //  it should """evaluate to a list of 3""" in {
  //
  //    def fVarArgs(ps: String*): Seq[String] = ps
  //
  //    val convertersVarArgs = List(s2s)
  //    val gfVarArgs = RenderableFunction(fVarArgs _, "varargs", RenderableFunction.callByValue(1))
  //    val ifVarArgs = InvocationFunction(gfVarArgs, Some(0), convertersVarArgs)
  //
  //    val rowData = Map[String, String]()
  //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
  //
  //    implicit val functionLibrary = FunctionLibrary.create(lookup) + ("varargs" -> ifVarArgs)
  //    val variables = List(lqss("A"), lqss("B"), lqss("C"))
  //    val invocation = InvocationPn1(variables)
  //    val by: Try[Seq[String]] = for (c1 <- invocation.asClosure[String, Seq[String]]; c2 <- c1.asClosure[String]; b <- c2()) yield b
  //    by should matchPattern { case Success(Seq("A", "B", "C")) => }
  //  }

  //  it should """evaluate to a list of 10""" in {
  //    def fVarArgs(ps: String*): Seq[String] = ps
  //
  //    val convertersVarArgs = List(s2s)
  //    val gfVarArgs = RenderableFunction(fVarArgs _, "varargs", RenderableFunction.callByValue(1))
  //    val ifVarArgs = InvocationFunction(gfVarArgs, Some(0), convertersVarArgs)
  //
  //    val rowData = Map[String, String]()
  //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
  //
  //    implicit val functionLibrary = FunctionLibrary.create(lookup) + ("varargs" -> ifVarArgs)
  //    val variables = List(lqss("A"), lqss("B"), lqss("C"), lqss("D"), lqss("E"), lqss("F"), lqss("G"), lqss("H"), lqss("I"), lqss("J"))
  //    val invocation = InvocationPn1(variables)
  //    val c2y: Try[Closure[_, Seq[String]]] = for (c1 <- invocation.asClosure[String, Seq[String]]; c2 <- c1.asClosure[String]) yield c2
  //    c2y foreach (_.render() shouldBe "Closure(RenderableFunction[List(),scala.collection.Seq](0,  mkList), )")
  //    val by: Try[Seq[String]] = for (c2 <- c2y; b <- c2()) yield b
  //    by should matchPattern { case Success(Seq("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")) => }
  //  }

  private def lqss(s: String) = Left(qss(s))

  private def qss(s: String) = QuotedStringScalar(s)

  behavior of "InvocationFP"
  //  it should """evaluate to false with NOT TRUE""" in {
  //    def fNot(p1: Boolean): Boolean = !p1
  //
  //    val paramsNot = List("boolean")
  //    val convertersNot = List(s2b)
  //    val gfNot = RenderableFunction(fNot _, FunctionString.custom("not", paramsNot), RenderableFunction.callByValue(1))
  //    val ifNot = InvocationFunction(gfNot, None, convertersNot)
  //
  //    val rowData = Map("k" -> "K")
  //    val getValue: Lookup[String] = { () => mapLookup(rowData) }
  //    implicit val functionLibrary = FunctionLibrary.create(getValue) + ("not" -> ifNot)
  //    val invocation = InvocationFP("NOT", Left(BooleanScalar("TRUE")))
  //
  //    val by: Try[Boolean] = for (c1 <- invocation.asClosure[String, Boolean]; c2 <- c1.asClosure[Boolean]; b <- c2()) yield b
  //    by should matchPattern { case Success(false) => }
  //  }

  behavior of "InvocationPF"
  //  it should """evaluate to true with "B" IN ('A','B','C')""" in {
  //    def fVarArgs(ps: String*): Seq[String] = ps
  //
  //    val convertersVarArgs = List(s2s)
  //    val gfVarArgs = RenderableFunction(fVarArgs _, "varargs", RenderableFunction.callByValue(1))
  //    val ifVarArgs = InvocationFunction(gfVarArgs, Some(0), convertersVarArgs)
  //
  //    def fIn(p0: String, p1: List[String]): Boolean = p1.contains(p0)
  //
  //    val paramsIn = List("x", "list")
  //    val convertersIn = List(s2s, s2s)
  //    val gfIn = RenderableFunction(fIn _, FunctionString.custom("in", paramsIn), RenderableFunction.callByValue(2))
  //    val ifIn = InvocationFunction(gfIn, None, convertersIn)
  //
  //    val rowData = Map("k" -> "K")
  //    val getValue: Lookup[String] = { () => mapLookup(rowData) }
  //    implicit val functionLibrary = FunctionLibrary.create(getValue) + ("in" -> ifIn) + ("varargs" -> ifVarArgs)
  //    val invocation0 = InvocationPn1(List(lqss("A"), lqss("B"), lqss("C")))
  //    val invocation1 = InvocationFPn("IN", List(Right(invocation0)))
  //    val invocation2 = InvocationPF(lqss("B"), invocation1)
  //
  //    val by: Try[Boolean] = for (c1 <- invocation2.asClosure[String, Boolean]; c2 <- c1.asClosure[String]; b <- c2()) yield b
  //    by should matchPattern { case Success(true) => }
  //  }

  behavior of "InvocationBoolean"
  //  it should """evaluate to true with true & true""" in {
  //
  //    def fAnd(p1: Boolean, p2: => Boolean): Boolean = p1 && p2
  //
  //    def fOr(p1: Boolean, p2: => Boolean): Boolean = p1 || p2
  //
  //    val ifAnd = InvocationFunction[Boolean](RenderableFunction(fAnd _, "and", List(false, true)), None, List(s2b, s2b))
  //    val ifOr = InvocationFunction[Boolean](RenderableFunction(fOr _, "or", List(false, true)), None, List(s2b, s2b))
  //
  //
  //    val rowData = Map("k" -> "K")
  //    val getValue: Lookup[Any] = { () => mapLookup(rowData) }
  //
  //    implicit val functionLibrary = FunctionLibrary.create(getValue) + ("and" -> ifAnd) + ("or" -> ifOr)
  //    val i = InvocationBoolean(And, Left(true), Left(true))
  //    val by: Try[Closure[_, Boolean]] = for (c1 <- i.asClosure[String, Boolean]; _ = Spy.log(s"c1: $c1"); c2 <- c1.asClosure[String]; _ = Spy.log(s"c2: $c2")) yield c2
  //    by foreach (_.render() shouldBe "Closure(RenderableFunction[List(Boolean, scala.Function0),Boolean](2, => and(a?)(=>b?)), \n  Left(true),\n  Left(true)\n)")
  //    Spy.log(s"by: $by")
  //    for (b <- by) yield b() should matchPattern { case Success(true) => }
  //    //    by should matchPattern { case Success(true) => }
  //  }

  it should """evaluate to false with x NOT IN ('A','B','C')""" in {
    //    def fVarArgs(ps: String*): Seq[String] = ps
    //
    //    val convertersVarArgs = List(s2s)
    //    val gfVarArgs = RenderableFunction(fVarArgs _, "varargs", RenderableFunction.callByValue(1))
    //    val ifVarArgs = InvocationFunction(gfVarArgs, Some(0), convertersVarArgs)
    //
    //    def fIn(p0: String, p1: List[String]): Boolean = p1.contains(p0)
    //
    //    val paramsIn = List("x", "list")
    //    val convertersIn = List(s2s, s2s)
    //    val gfIn = RenderableFunction(fIn _, FunctionString.custom("in", paramsIn), RenderableFunction.callByValue(2))
    //    val ifIn = InvocationFunction(gfIn, None, convertersIn)
    //
    //    def fNot(p1: Boolean): Boolean = !p1
    //
    //    val paramsNot = List("b")
    //    val convertersNot = List(s2b)
    //    val gfNot = RenderableFunction(fNot _, FunctionString.custom("not", paramsNot), RenderableFunction.callByValue(1))
    //    val ifNot = InvocationFunction(gfNot, None, convertersNot)
    //    val rowData = Map("k" -> "K")
    //    val lookup: Lookup[String] = { () => mapLookup(rowData) }
    //    implicit val functionLibrary = FunctionLibrary.create(lookup) + ("not" -> ifNot) + ("in" -> ifIn) + ("varargs" -> ifVarArgs)
    //    val invocation0 = InvocationPn1(List(lqss("A"), lqss("B"), lqss("C")))
    //    val invocation1 = InvocationFPn("IN", List(Right(invocation0)))
    //    val invocation2 = InvocationFP("NOT", Right(invocation1))
    //    val invocation3 = InvocationPF(lqss("X"), invocation2)
    //
    //    val cy = invocation3.asClosure[String, Boolean]
    //    (for (c <- cy; z <- c.asClosure[Boolean]; b <- z()) yield b) should matchPattern { case Success(true) => }
  }

  behavior of "InvocationBooleanExpression"
  //  it should """evaluate to true""" in {
  //
  //    implicit val functionLibrary = CriteriaEvaluator.createFunctionLibrary(MockUDF)
  //    val invocation = InvocationBooleanExpression(Left(Scalar(true)), List(BooleanTerm(And, Left(Scalar(true)))))
  //    val cy = invocation.asClosure[Boolean, Boolean]
  //    (for (c <- cy; z <- c.asClosure[Boolean]; b <- z()) yield b) should matchPattern { case Success(true) => }
  //  }

  behavior of "InvocationComparison"
  it should """evaluate 1<2 as true""" in {
    testComparison("<", 1, 2, result = true)
  }
  it should """evaluate 1>2 as false""" in {
    testComparison(">", 1, 2, result = false)
  }
  it should """evaluate 1<=2 as true""" in {
    testComparison("<=", 1, 2, result = true)
  }
  it should """evaluate 1>=1 as true""" in {
    testComparison(">=", 1, 1, result = true)
  }
  it should """evaluate 1<>2 as true""" in {
    testComparison("<>", 1, 2, result = true)
  }
  it should """evaluate 1!=2 as true""" in {
    testComparison("!=", 1, 2, result = true)
  }
  it should """evaluate 1<=1 as true""" in {
    testComparison("<=", 1, 1, result = true)
  }

  private def testComparison(op: String, x: Int, y: Int, result: Boolean): Unit = {
    val rowData = Map("x" -> x, "y" -> y)
    val lookup: Lookup[Int] = { () => mapLookup(rowData) }
    val invocation = InvocationComparison(Right(InvocationLookup("x")), op, Right(InvocationLookup("y")))

  }

  behavior of "InvocationWhenThen"
  //  it should """evaluate when true then 'A'""" in {
  //    implicit val functionLibrary = CriteriaEvaluator.createFunctionLibrary(MockUDF)
  //    val invocation = InvocationWhenThen(InvocationP(Left(Scalar(true))), Left(QuotedStringScalar("A")))
  //    val cy = invocation.asClosure[String, String]
  //    (for (c <- cy; z <- c.asClosure[String]; b <- z()) yield b) should matchPattern { case Success("A") => }
  //  }
  //  it should """evaluate when false then 'A'""" in {
  //    implicit val functionLibrary = CriteriaEvaluator.createFunctionLibrary(MockUDF)
  //    val invocation = InvocationWhenThen(InvocationP(Left(Scalar(false))), Left(QuotedStringScalar("A")))
  //    val cy = invocation.asClosure[String, String]
  //    (for (c <- cy; z <- c.asClosure[String]; b <- z()) yield b) should matchPattern { case Success(null) => }
  //  }

  behavior of "InvocationGetOrElse"
  //  it should """evaluate when true then 'A'""" in {
  //    implicit val functionLibrary = CriteriaEvaluator.createFunctionLibrary(MockUDF)
  //    val invocation = InvocationGetOrElse(InvocationWhenThen(InvocationP(Left(Scalar(true))), Left(QuotedStringScalar("A"))), InvocationP(Left(QuotedStringScalar("B"))))
  //    val cy = invocation.asClosure[String, String]
  //    (for (c <- cy; z <- c.asClosure[String]; b <- z()) yield b) should matchPattern { case Success("A") => }
  //  }
  //  it should """evaluate when false then 'A'""" in {
  //    implicit val functionLibrary = CriteriaEvaluator.createFunctionLibrary(MockUDF)
  //    val invocation = InvocationGetOrElse(InvocationWhenThen(InvocationP(Left(Scalar(false))), Left(QuotedStringScalar("A"))), InvocationP(Left(QuotedStringScalar("B"))))
  //    val cy = invocation.asClosure[String, String]
  //    (for (c <- cy; z <- c.asClosure[String]; b <- z()) yield b) should matchPattern { case Success("B") => }
  //  }
  //
  //  behavior of "InvocationCaseClause"
  //  it should "work" in {
  //    implicit val functionLibrary = CriteriaEvaluator.createFunctionLibrary(MockUDF)
  //    val invocation = InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Left(Scalar(true)), List()), Left(Scalar("x")))), Some(Left(Scalar("y"))))
  //    val cy = invocation.asClosure[String, String]
  //    (for (c <- cy; z <- c.asClosure[String]; b <- z()) yield b) should matchPattern { case Success("x") => }
  //
  //  }

  behavior of "render"
  it should "work for InvocationBooleanExpression" in {
    InvocationBooleanExpression(Left(Scalar("x")), List(BooleanTerm(And, Left("y")))).render(0) shouldBe "boolean(Left(x) AND y)"
  }
  it should "work for InvocationBoolean" in {
    InvocationBoolean(And, Left(Scalar("x")), Left(Scalar("y"))).render(0) shouldBe "AND(\n    Left(x),\n    Left(y)\n  )"
  }
  it should "work for InvocationComparison" in {
    InvocationComparison(Left(Scalar("x")), ">", Left(Scalar("y"))).render() shouldBe "compare(\n    Left(gt),\n    Left(x),\n    Left(y)\n  )"
  }
  it should "work for InvocationFP" in {
    InvocationFP("concat", Left(Scalar("x"))).render() shouldBe "concat(Left(x))"
  }
  it should "work for InvocationP" in {
    InvocationP(Left(Scalar("x"))).render() shouldBe "identity(Left(x))"
  }
  it should "work for InvocationFPn" in {
    InvocationFPn("hello", List(Left(Scalar("x")), Left(Scalar("y")))).render() shouldBe "hello(\n    Left(x),\n    Left(y)\n  )"
  }
  it should "work for InvocationPn1" in {
    InvocationPn1(List(Left(Scalar("x")), Left(Scalar("y")))).render() shouldBe "varargs(\n    Left(x),\n    Left(y)\n  )"
  }
  it should "work for InvocationPF" in {
    InvocationPF(Right(InvocationLookup("x")), InvocationFP("concat", Left(Scalar("y")))).render() shouldBe "InvocationPF(\n  Right(lookup(Left(x))) concat(Left(y)))"
  }
  it should "work for InvocationWhenThen" in {
    InvocationWhenThen(InvocationBooleanExpression(Left(Scalar("x")), List(BooleanTerm(And, Left("y")))), Left(Scalar("x"))).render() shouldBe "whenThen(\n    Right(boolean(Left(x) AND y)),\n    Left(x)\n  )"
  }
  it should "work for InvocationCaseClause with Else" in {
    InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Left(Scalar("x")), List(BooleanTerm(And, Left("y")))), Left(Scalar("x")))), Some(Left(Scalar("y")))).render() shouldBe "case(\n    Right(whenThen(\n          Right(boolean(Left(x) AND y)),\n          Left(x)\n        )),\n    Left(y)\n  )"
  }
  it should "work for InvocationCaseClause without Else" in {
    InvocationCaseClause(List(InvocationWhenThen(InvocationBooleanExpression(Left(Scalar("x")), List(BooleanTerm(And, Left("y")))), Left(Scalar("x")))), None).render() shouldBe "case(Right(whenThen(\n          Right(boolean(Left(x) AND y)),\n          Left(x)\n        )))"
  }
}
