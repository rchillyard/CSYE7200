/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

class ArgsSpec extends AnyFlatSpec with Matchers {

  private val argFilename = "argFilename"
  private val nameF = "f"
  private val cmdF = "-" + nameF
  private val sX = "x"
  private val sY = "y"
  private val s1 = "1"
  private val x1 = 1

  private def printFilename(so: Option[String]): Unit = so.foreach(s => println(s"$argFilename: $s"))

  val processor: Map[String, Option[String] => Unit] = Map[String, Option[String] => Unit](nameF -> printFilename)

  behavior of "Arg"

  it should "work for " + sX + ": " + s1 in {
    val target = Arg(sX, s1)
    target.name shouldBe Some(sX)
    target.value shouldBe Some(s1)
  }

  it should "implement map" in {
    val target = Arg(sX, s1)
    val result = target.map(_.toInt)
    result.value shouldBe Some(x1)
  }

  it should "implement map with exception" in {
    val target = Arg(sX, sX)
    a[java.lang.NumberFormatException] shouldBe thrownBy(target.map(_.toInt))
  }

  it should "implement toY" in {
    implicit object DerivableStringInt$ extends Derivable[Int] {

      def deriveFrom[X](x: X): Int = x match {
        case x: String => x.toInt
        case _ => throw NotImplemented(s"deriveFrom: $x")
      }
    }
    val target = Arg(sX, s1)
    target.toY[Int] shouldBe 1
  }

  it should "implement byName" in {
    val target = Arg(sX, sX)
    target.byName(sX) should matchPattern { case Some(_) => }
  }

  it should "process " + sX + ": append" in {
    val sb = new StringBuilder
    val processor = Map[String, Option[String] => Unit](sX ->[Option[String] => Unit] { x => sb.append(x) })
    val target = Arg(sX, s1)
    val result = target.process(processor)
    result should matchPattern { case Success(_) => }
    sb.toString shouldBe "Some(" + s1 + ")"
  }

  it should "not process " + sY + ": append" in {
    val sb = new StringBuilder
    val processor = Map[String, Option[String] => Unit](sX ->[Option[String] => Unit] { x => sb.append(x) })
    val target = Arg(sY, s1)
    val result = target.process(processor)
    result should matchPattern { case Failure(_) => }
  }

  behavior of "Args"

  it should "work" in {
    val target = Args.create(Arg(sX, s1))
    target.size shouldBe 1
    target.head.name shouldBe Some(sX)
    target.head.value shouldBe Some(s1)
  }

  it should "implement map" in {
    val target = Args.create(Arg(sX, s1))
    val result = target.map { x: String => x.toInt }
    result.head.value shouldBe Some(x1)
  }

  it should "implement getArg with good name" in {
    val x = Arg(sX, s1)
    val target = Args.create(x)
    val result = target.getArg(sX)
    result shouldBe Some(`x`)
  }

  it should "not implement getArg with bad name" in {
    val x = Arg(sX, s1)
    val target = Args.create(x)
    val result = target.getArg("")
    result shouldBe None
  }

  it should "not implement getArg with ambiguous name" in {
    val x = Arg(sX, s1)
    val target = Args.create(x, x)
    a[AmbiguousNameException] shouldBe thrownBy(target.getArg(sX))
  }

  it should "implement extract" in {
    val target = Args.create(Arg(sX, s1))
    target.extract shouldBe Map(sX -> Some(s1))
  }

  it should "process " + sX + ": append" in {
    val sA = "a"
    val sb = new StringBuilder
    val processor = Map[String, Option[String] => Unit](sX ->[Option[String] => Unit] { case Some(x) => sb.append(x); case _ => })
    val target = Args.create(Arg(sX, s1), Arg(sX, sA))
    val result = target.process(processor)
    result should matchPattern { case Success(_) => }
    sb.toString shouldBe s1 + sA
  }

  it should "parse " + cmdF + " " + argFilename in {
    val args = Array(cmdF, argFilename)
    val as = Args.parse(args)
    as.xas.length shouldBe 1
    as.xas.head shouldBe Arg(Some(nameF), Some(argFilename))
  }

  it should "parse 1 2 3" in {
    val sa = Args.parse(Array("1", "2", "3"))
    sa.xas.length shouldBe 3
    sa.xas.head shouldBe Arg(None, Some("1"))
    val xa = sa.map { x: String => x.toInt }
    xa shouldBe Args(Seq(Arg(None, Some(1)), Arg(None, Some(2)), Arg(None, Some(3))))
    val processor = Map[String, Option[Int] => Unit]()
    xa.process(processor) should matchPattern { case Success(Seq(1, 2, 3)) => }
  }

  it should """parse "1 a" "2:x" 3""" in {
    val sa = Args.parse(Array(""""1 a"""", """"2:x"""", "3"))
    sa.xas.length shouldBe 3
    sa.xas.head shouldBe Arg(None, Some("1 a"))
  }

  it should "parsePosix 1 2 3" in {
    val sa = Args.parse(Array("1", "2", "3"))
    sa.xas.length shouldBe 3
    sa.xas.head shouldBe Arg(None, Some("1"))
    val xa = sa.map { x: String => x.toInt }
    xa shouldBe Args(Seq(Arg(None, Some(1)), Arg(None, Some(2)), Arg(None, Some(3))))
    val processor = Map[String, Option[Int] => Unit]()
    xa.process(processor) should matchPattern { case Success(Seq(1, 2, 3)) => }
  }

  it should """parsePosix "-xf argFilename 3.1415927"""" in {
    val sa = Args.parsePosix(Array("-xf", "argFilename", "3.1415927"))
    sa.xas.length shouldBe 3
    sa.xas.head shouldBe Arg(Some("x"), None)
    sa.xas.tail.head shouldBe Arg(Some("f"), Some("argFilename"))
    sa.xas.last shouldBe Arg(None, Some("3.1415927"))
  }

  it should """parsePosix "-xf argFilename -p 3.1415927"""" in {
    val sa = Args.parsePosix(Array("-xf", "argFilename", "-p", "3.1415927"))
    sa.xas.length shouldBe 3
    sa.xas.head shouldBe Arg(Some("x"), None)
    sa.xas.tail.head shouldBe Arg(Some("f"), Some("argFilename"))
    sa.xas.last shouldBe Arg(Some("p"), Some("3.1415927"))
  }

  it should """implement isDefined("x")""" in {
    val sa = Args[String](Seq(Arg(Some("x"), None), Arg(Some("f"), Some("argFilename")), Arg(None, Some("3.1415927"))))
    sa.isDefined("x") shouldBe true
  }

  it should """implement getArgValue("f")""" in {
    implicit object DerivableStringString$ extends Derivable[String] {

      def deriveFrom[X](x: X): String = x match {
        case x: String => x
        case _ => throw NotImplemented(s"deriveFrom: $x")
      }
    }
    val sa = Args[String](Seq(Arg(Some("x"), None), Arg(Some("f"), Some("argFilename")), Arg(None, Some("3.1415927"))))
    sa.getArgValue("f") shouldBe Some("argFilename")
  }

  it should """implement process for complex Args""" in {
    var x = false
    var filename = ""
    val sa = Args[String](Seq(Arg(Some("x"), None), Arg(Some("f"), Some("argFilename")), Arg(None, Some("3.1415927"))))
    val processor = Map[String, Option[String] => Unit]("x" -> { _ => x = true }, "f" -> { case Some(w) => filename = w; case _ => })
    sa.process(processor) should matchPattern { case Success(Seq("3.1415927")) => }
    x shouldBe true
    filename shouldBe "argFilename"
  }

  behavior of "SimpleArgParser"

  private val p = SimpleArgParser

  it should "parse command " + cmdF in {
    p.parseAll(p.command, cmdF) should matchPattern { case p.Success(p.Command(`nameF`), _) => }
  }

  it should "not parse command -X" in {
    p.parseAll(p.command, "-X") should matchPattern { case p.Failure(_, _) => }
  }

  it should "parse argument " + argFilename in {
    p.parseAll(p.argument, argFilename) should matchPattern { case p.Success(p.Argument(`argFilename`), _) => }
  }

  it should "not parse argument -x" in {
    p.parseAll(p.argument, "-x") should matchPattern { case p.Failure(_, _) => }
  }

  it should "parse token " + cmdF in {
    p.parseAll(p.token, cmdF) should matchPattern { case p.Success(p.Command(`nameF`), _) => }
  }

  it should "parse token " + argFilename in {
    p.parseAll(p.token, argFilename) should matchPattern { case p.Success(p.Argument(`argFilename`), _) => }
  }

  it should "parse " + cmdF in {
    p.parseToken(cmdF) should matchPattern { case Success(p.Command(`nameF`)) => }
  }

  it should "parse " + argFilename in {
    p.parseToken(argFilename) should matchPattern { case Success(p.Argument(`argFilename`)) => }
  }

  it should """parse "x y" as an argument""" in {
    val parser = p
    parser.parseAll(parser.argument, """"x y"""") should matchPattern { case parser.Success(_, _) => }
  }

  it should """parse "x y" as a token""" in {
    val parser = p
    parser.parseAll(parser.token, """"x y"""") should matchPattern { case parser.Success(_, _) => }
    """"/Users/rhillyard/Downloads/Mid-term Exam.download.xls-2.csv""""
  }

  it should """parse "/Users..." as a token""" in {
    p.parseAll(p.token, """"/Users/rhillyard/Downloads/Mid-term Exam.download.xls-2.csv"""") should matchPattern { case p.Success(_, _) => }
  }

  behavior of "PosixArgParser"

  it should "parse options from -xf;" in {
    val p = PosixArgParser
    val pp: p.ParseResult[PosixArg] = p.parseAll(p.posixOptions, "-xf;")
    pp should matchPattern { case p.Success(_, _) => }
  }

  it should "parse operand from 3.1415927;" in {
    val p = PosixArgParser
    val pp: p.ParseResult[PosixArg] = p.parseAll(p.posixOperand, "3.1415927;")
    pp should matchPattern { case p.Success(_, _) => }
  }

  it should "parse option value from -argFilename;" in {
    val p = PosixArgParser
    val pp: p.ParseResult[PosixArg] = p.parseAll(p.posixOptionValue, "argFilename;")
    pp should matchPattern { case p.Success(_, _) => }
  }

  it should "parse option set from -xf;argFilename" in {
    val p = PosixArgParser
    val pp: p.ParseResult[Seq[PosixArg]] = p.parseAll(p.posixOptionSet, "-xf;argFilename;")
    pp should matchPattern { case p.Success(_, _) => }
    pp.get.size shouldBe 2
    pp.get.head shouldBe PosixOptions("xf")
    pp.get.last shouldBe PosixOptionValue("argFilename")
  }

  it should """parseCommandLine "-xf argFilename 3.1415927"""" in {
    val p = PosixArgParser
    val as: Seq[PosixArg] = p.parseCommandLine(Seq("-xf", "argFilename", "3.1415927"))
    as.size shouldBe 3
    as.head shouldBe PosixOptions("xf")
    as.tail.head shouldBe PosixOptionValue("argFilename")
    as.last shouldBe PosixOperand("3.1415927")
  }

  behavior of "PosixSynopsisParser"
  it should "parse x as OptionToken" in {
    val p = PosixSynopsisParser
    val cr = p.parse(p.command, "x")
    cr should matchPattern { case p.Success(p.Command("x"), _) => }
  }
  it should "parse xf as two optionTokens" in {
    val p = PosixSynopsisParser
    val eEr = p.parse(p.command ~ p.command, "xf")
    eEr should matchPattern { case p.Success(_, _) => }
  }
  it should """parse "filename" as ValueToken1""" in {
    val p = PosixSynopsisParser
    val wr = p.parse(p.valueToken1, "filename")
    wr should matchPattern { case p.Success("filename", _) => }
  }
  it should "parse Junk as ValueToken2" in {
    val p = PosixSynopsisParser
    val wr = p.parse(p.valueToken2, "Junk")
    wr should matchPattern { case p.Success("Junk", _) => }
  }
  it should "parse Junk as ValueToken" in {
    val p = PosixSynopsisParser
    val vr = p.parse(p.value, "Junk")
    vr should matchPattern { case p.Success(p.Value("Junk"), _) => }
  }
  it should """parse " filename" as ValueToken""" in {
    val p = PosixSynopsisParser
    val vr = p.parse(p.value, " filename")
    vr should matchPattern { case p.Success(p.Value("filename"), _) => }
  }
  it should "parse x as an optionOrValueToken" in {
    val p = PosixSynopsisParser
    val er = p.parse(p.optionWithOrWithoutValue, "x")
    er should matchPattern { case p.Success(_, _) => }
  }
  it should "parse xf as two optionOrValueTokens" in {
    val p = PosixSynopsisParser
    val eEr = p.parse(p.optionWithOrWithoutValue ~ p.optionWithOrWithoutValue, "xf")
    eEr should matchPattern { case p.Success(_, _) => }
  }
  it should "parse [f] as an optionalElement" in {
    val p = PosixSynopsisParser
    val er = p.parse(p.optionalElement, "[f]")
    er should matchPattern { case p.Success(_, _) => }
  }
  it should "parse x[f] as one optionOrValueToken with three characters left over" in {
    val p = PosixSynopsisParser
    val er = p.parse(p.optionWithOrWithoutValue, "x[f]")
    er should matchPattern { case p.Success(p.Command("x"), _) => }
    er.next.pos.column should matchPattern { case 2 => }
  }
  it should "parse x[f] as one optionOrValueToken followed by an optionalElement" in {
    val p = PosixSynopsisParser
    val eEr = p.parse(p.optionWithOrWithoutValue ~ p.optionalElement, "x[f]")
    eEr should matchPattern { case p.Success(_, _) => }
  }
  it should "parse x[f filename] as one optionOrValueToken followed by an optionalElement" in {
    val p = PosixSynopsisParser
    val eEr = p.parse(p.optionWithOrWithoutValue ~ p.optionalElement, "x[f filename]")
    eEr should matchPattern { case p.Success(_, _) => }
  }
  it should "parse f filename as a optionalOrRequiredElement" in {
    val p = PosixSynopsisParser
    val esr = p.parse(p.optionalOrRequiredElement, "f filename")
    esr should matchPattern { case p.Success(_, _) => }
  }
  it should "parse [xf filename] as a List[Element]" in {
    val p = PosixSynopsisParser
    val esr = p.parse(p.optionalElements, "[xf filename]")
    esr should matchPattern { case p.Success(_, _) => }
  }
  it should "parse -x[f filename] as a synopsis" in {
    val p = PosixSynopsisParser
    val so: Option[Synopsis] = p.parseSynopsis(Some("-x[f filename]"))
    so shouldBe Some(Synopsis(Seq(p.Command("x"), p.OptionalElement(p.CommandWithValue("f", p.Value("filename"))))))
  }
  it should "parse -[xf filename]" in {
    val p = PosixSynopsisParser
    val so: Option[Synopsis] = p.parseSynopsis(Some("-[xf filename]"))
    so shouldBe Some(Synopsis(Seq(p.OptionalElement(p.Command("x")), p.OptionalElement(p.CommandWithValue("f", p.Value("filename"))))))
  }

  behavior of "Args validation"
  it should "parse " + cmdF + " " + argFilename in {
    val args = Array(cmdF, argFilename, "positionalArg")
    val as: Args[String] = Args.parsePosix(args, Some(cmdF + " " + "filename"))
    as should matchPattern { case Args(_) => }
  }

  it should "parse " + cmdF + " " + argFilename + "where -xf filename required" in {
    a[ValidationException[String]] shouldBe thrownBy(Args.parsePosix(Array(cmdF, argFilename), Some("-xf filename")))
  }

}

case class NotImplemented(str: String) extends Exception(s"Not Implemented for $str")
