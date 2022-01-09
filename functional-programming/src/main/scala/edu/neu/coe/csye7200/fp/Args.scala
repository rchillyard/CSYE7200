/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.MonadOps

import scala.util._
import scala.util.parsing.combinator.RegexParsers

case class Arg[X](name: Option[String], value: Option[X]) extends Ordered[Arg[X]] {
  def isOptional(s: Synopsis): Boolean = s.find(name) match {
    case Some(e) => e.isOptional
    case _ => throw InvalidOptionException(this)
  }


  def byName(w: String): Option[Arg[X]] = name match {
    case Some(`w`) => Some(this)
    case _ => None
  }

  def map[Y](f: X => Y): Arg[Y] = Arg(name, value map f)

  lazy val asMaybeTuple: Option[(String, Option[X])] = name match {
    case Some(w) => Some(w, value)
    case _ => None
  }

  def toY[Y: Derivable]: Y = value match {
    case Some(x) => implicitly[Derivable[Y]].deriveFrom(x)
    case _ => throw NoValueException(name)
  }

  def process(fm: Map[String, Option[X] => Unit]): Try[Option[X]] = {
    def processFuncMaybe(fo: Option[Option[X] => Unit]): Try[Option[X]] = fo match {
      case Some(f) => Try(f(value)).map(_ => None)
      case None => Failure(AnonymousNotFoundException)
    }

    def process(c: String): Try[Option[X]] = processFuncMaybe(fm.get(c)).recoverWith({ case AnonymousNotFoundException => Failure(NotFoundException(c)) })

    name match {
      case Some(c) => process(c)
      case None => Success(value)
    }
  }

  override def toString: String = s"Arg: command ${name.getOrElse("anonymous")} with value: ${value.getOrElse("none")}"

  def compare(that: Arg[X]): Int = name match {
    case Some(x) => that.name match {
      case Some(y) => x compare y
      case None => throw CompareException(s"$this vs $that")
    }
    case None => throw CompareException(s"$this vs $that")
  }
}

object Arg {
  def apply(c: String): Arg[String] = Arg(Some(c), None)

  def apply(c: String, a: String): Arg[String] = Arg(Some(c), Some(a))
}

case class Args[X](xas: Seq[Arg[X]]) extends Iterable[Arg[X]] {

  def validate(w: String): Args[X] = validate(PosixSynopsisParser.parseSynopsis(Some(w)))

  def validate(so: Option[Synopsis]): Args[X] = so match {
    case Some(s) => if (validate(s)) this else throw ValidationException(this, s)
    case _ => this
  }

  def validate(s: Synopsis): Boolean = {
    val (m, _) = s.mandatoryAndOptionalElements
    // NOTE: the following will throw an exception if any Arg is invalid
    val (_, mandatory) = xas.filter(_.name.isDefined).partition(_.isOptional(s))
    if (m.size == mandatory.size) {
      val bs = for (z <- m.sorted zip mandatory.sorted) yield (z._1.value compare z._2.name.get) == 0
      bs.forall(_ == true)
    }
    else
      false
  }

  def validate(e: Element): Boolean = false

  /**
    * Apply the given function f to each Arg of this Args
    *
    * @param f a function of type X => Y
    * @tparam Y the result type of the function f
    * @return an Args[Y] object
    */
  def map[Y](f: X => Y): Args[Y] = Args(for (xa <- xas) yield xa.map(f))

  /**
    * Get the options (i.e. args with names) as map of names to (optional) values
    *
    * @return the options as a map
    */
  def extract: Map[String, Option[X]] = (for (xa <- xas) yield xa.asMaybeTuple).flatten.toMap

  /**
    * Method to get an Arg whose name matches the given string.
    *
    * @param w the string to match
    * @return Some(arg) if the name matches, else None
    */
  def getArg(w: String): Option[Arg[X]] = (for (xa <- xas) yield xa.byName(w)).flatten.toList match {
    case xa :: Nil => Some(xa)
    case Nil => None
    case _ => throw AmbiguousNameException(w)
  }

  /**
    * Get the arg value where the name matches the given string and where the resulting type is Y
    *
    * @param w the string to match
    * @tparam Y the result type
    * @return an option value of Y
    */
  def getArgValue[Y: Derivable](w: String): Option[Y] = getArg(w) map (xa => xa.toY)

  def isDefined(w: String): Boolean = getArg(w).isDefined

  def process(fm: Map[String, Option[X] => Unit]): Try[Seq[X]] =
  // CONSIDER using traverse
    MonadOps.sequence(for (xa <- xas) yield for (x <- xa.process(fm)) yield x) match {
      case Success(xos) => Success(xos.flatten)
      case Failure(x) => Failure(x)
    }

  def iterator: Iterator[Arg[X]] = xas.iterator

  override def foreach[U](f: Arg[X] => U): Unit = xas foreach f

}

object Args {
  def create(args: Arg[String]*): Args[String] = apply(args)

  def parse(args: Array[String]): Args[String] = {
    val p = SimpleArgParser

    @scala.annotation.tailrec
    def inner(r: Seq[Arg[String]], w: Seq[p.Token]): Seq[Arg[String]] = w match {
      case Nil => r
      case p.Command(c) :: p.Argument(a) :: t => inner(r :+ Arg(c, a), t)
      case p.Command(c) :: t => inner(r :+ Arg(c), t)
      case p.Argument(a) :: t => inner(r :+ Arg(None, Some(a)), t)
    }

    // CONSIDER using traverse
    val tys = for (a <- args) yield p.parseToken(a)
    val ts = MonadOps.sequence(tys) match {
      case Success(ts_) => ts_
      case Failure(x) => System.err.println(x.getLocalizedMessage); Seq[p.Token]()
    }
    Args(inner(Seq(), ts))
  }

  def parsePosix(args: Array[String], synopsis: Option[String] = None): Args[String] = doParse(PosixArgParser.parseCommandLine(args), synopsis)

  private def doParse(ps: Seq[PosixArg], synopsis: Option[String] = None): Args[String] = {
    def processPosixArg(p: PosixArg): Seq[PosixArg] = p match {
      case PosixOptions(w) => for (c <- w) yield PosixOptions(c.toString)
      case x => Seq(x)
    }

    @scala.annotation.tailrec
    def inner(r: Seq[Arg[String]], w: Seq[PosixArg]): Seq[Arg[String]] = w match {
      case Nil => r
      case PosixOptions(o) :: PosixOptionValue(v) :: t => inner(r :+ Arg(o, v), t)
      case PosixOptions(o) :: t => inner(r :+ Arg(o), t)
      case PosixOperand(o) :: t => inner(r :+ Arg(None, Some(o)), t)
    }

    val eso = PosixSynopsisParser.parseSynopsis(synopsis)
    val pss = for (p <- ps) yield processPosixArg(p)
    Args(inner(Seq(), pss.flatten)).validate(eso)
  }

}

/**
  * Type-class trait to allow conversion from type X to type T
  *
  * @tparam T the result type
  */
trait Derivable[T] {
  /**
    * Method to convert an X to a T
    *
    * @param x the X value
    * @tparam X the input type
    * @return a T
    */
  def deriveFrom[X](x: X): T
}

abstract class SimpleArgParser extends RegexParsers {
  def parseToken(s: String): Try[Token] = parseAll(token, s) match {
    case Success(t, _) => scala.util.Success(t)
    case Failure(msg, input) => scala.util.Failure(new Exception(s"Failure: could not parse '$s' as a token because: $msg with input pos=${input.pos}"))
    case Error(msg, input) => scala.util.Failure(new Exception(s"Error: could not parse '$s' as a token because: $msg with input pos=${input.pos}"))
  }

  trait Token {
    def s: String
  }

  case class Command(s: String) extends Token

  case class Argument(s: String) extends Token

  def token: Parser[Token] = command | argument

  def command: Parser[Command] = "-" ~> cmdR ^^ (s => Command(s))

  def argument: Parser[Argument] = argR ^^ (s => Argument(s))

  private val cmdR = """[a-z]+""".r
  private val doubleQuote = """""""
  private val argR = doubleQuote ~> """[^"]*""".r <~ doubleQuote | """[^-].*""".r | failure("invalid argument")
}

object SimpleArgParser extends SimpleArgParser

trait PosixArg {
  def value: String
}

/**
  * One or more options.
  * Each option is a single-letter.
  *
  * @param value the string of options, without the "-" prefix.
  */
case class PosixOptions(value: String) extends PosixArg

/**
  * The value of the preceding option.
  *
  * @param value a String
  */
case class PosixOptionValue(value: String) extends PosixArg

/**
  * The value of an operand, i.e. a String which follows all of the options and their values.
  *
  * @param value a String
  */
case class PosixOperand(value: String) extends PosixArg

object PosixArgParser extends RegexParsers {

  def flatten[T](to: Option[List[T]]): List[T] = to.toList.flatten

  def parseCommandLine(ws: Seq[String]): Seq[PosixArg] = parseAll(posixCommandLine, ws.mkString("", terminator, terminator)) match {
    case Success(t, _) => t
    case _ => throw ParseException(s"could not parse '$ws' as a token")
  }

  /**
    * Note that it is impossible to tell whether the first arg after an option set is an option value or the first operand.
    * Only validating it with a command line synopsis can do that for sure.
    *
    * @return
    */
  def posixCommandLine: Parser[Seq[PosixArg]] = rep(posixOptionSet) ~ rep(posixOperand) ^^ { case pss ~ ps => pss.flatten ++ ps }

  def posixOptionSet: Parser[Seq[PosixArg]] = posixOptions ~ opt(posixOptionValue) ^^ { case p ~ po => p +: po.toSeq }

  def posixOptions: Parser[PosixArg] = "-" ~> """[a-z0-9]+""".r <~ terminator ^^ (s => PosixOptions(s))

  def posixOptionValue: Parser[PosixArg] = nonOption ^^ (s => PosixOptionValue(s))

  def posixOperand: Parser[PosixArg] = nonOption ^^ (s => PosixOperand(s))

  def nonOption: Parser[String] = """[^;]+""".r <~ terminator

  val terminator = ";"
}

/**
  * This represents an element in the synopsis for a command line
  */
trait Element extends Ordered[Element] {
  def value: String

  def isOptional: Boolean = false

  def compare(that: Element): Int = value compare that.value
}

/**
  * Don't think we need this
  */
trait Optional

case class Synopsis(es: Seq[Element]) {
  def find(wo: Option[String]): Option[Element] = wo match {
    case Some(w) => es.find(e => e.value == w)
    case _ => None
  }

  def mandatoryAndOptionalElements: (Seq[Element], Seq[Element]) = es partition (!_.isOptional)
}

object PosixSynopsisParser extends RegexParsers {
  def parseSynopsis(wo: Option[String]): Option[Synopsis] = wo match {
    case Some(w) => parseAll(synopsis, w) match {
      case Success(es, _) => Some(Synopsis(es))
      case _ => throw new Exception(s"could not parse '$w' as a synopsis")
    }
    case _ => None
  }

  override def skipWhitespace: Boolean = false

  /**
    * This represents an "Option" in the parlance of POSIX command line interpretation
    *
    * @param value the (single-character) String representing the command
    */
  case class Command(value: String) extends Element

  /**
    * This represents an Option Value in the parlance of POSIX.
    *
    * @param value the String
    */
  case class Value(value: String) extends Element

  /**
    * This represents an "Option" and its "Value"
    *
    * @param value   the command or "option" String
    * @param element the Element which corresponds to the "value" of this synopsis command (and which may of course be OptionalElement).
    */
  case class CommandWithValue(value: String, element: Element) extends Element

  /**
    * This represents an optional synopsis element, either an optional command, or an optional value.
    *
    * @param element a synopsis element that is optional
    */
  case class OptionalElement(element: Element) extends Element with Optional {
    def value: String = element.value

    override def isOptional: Boolean = true
  }

  /**
    * A "synopsis" of command line options and their potential argument values.
    * It matches a dash ('-') followed by a list of optionalOrRequiredElement OR: an optional list of optionWithOrWithoutValue
    *
    * @return a Parser[Seq[Element]
    */
  def synopsis: Parser[Seq[Element]] = "-" ~> (optionalElements | rep(optionalOrRequiredElement))

  /**
    * An optionalOrRequiredElement matches EITHER: an optionalElement OR: an optionWithOrWithoutValue
    *
    * @return a Parser[Element] which is EITHER: a Parser[Command] OR: a Parser[CommandWithValue] OR: a Parser[OptionalElement]
    */
  def optionalOrRequiredElement: Parser[Element] = optionalElement | optionWithOrWithoutValue

  /**
    * An optionalElement matches a '[' followed by an optionWithOrWithoutValue followed by a ']'
    *
    * @return a Parser[OptionalElement]
    */
  def optionalElement: Parser[Element] = openBracket ~> optionWithOrWithoutValue <~ closeBracket ^^ (t => OptionalElement(t))

  /**
    * An optionalElement matches a '[' followed by an optionWithOrWithoutValue followed by a ']'
    *
    * @return a Parser[Seq[Element]
    */
  def optionalElements: Parser[Seq[Element]] = openBracket ~> rep(optionWithOrWithoutValue) <~ closeBracket ^^ (ts => for (t <- ts) yield OptionalElement(t))

  /**
    * An optionWithOrWithoutValue matches EITHER: an command; OR: an command followed by a value
    *
    * @return a Parser[Element] which is EITHER: a Parser[Command] OR: a Parser[CommandWithValue]
    */
  def optionWithOrWithoutValue: Parser[Element] = (command ~ value | command) ^^ {
    case o: Element => o
    case (o: Element) ~ (v: Element) => CommandWithValue(o.value, v)
    case _ => throw new Exception("")
  }

  /**
    * A value matches EITHER: a space [which is ignored] followed by a valueToken1 OR: a valueToken2
    *
    * @return a Parser[Value]
    */
  def value: Parser[Value] = ("""\s""".r ~> valueToken1 | valueToken2) ^^ (t => Value(t))

  /**
    * A command matches a single character which is either a lowercase letter or a digit
    *
    * @return a Parser[Command]
    */
  def command: Parser[Command] = """[\p{Ll}\d]""".r ^^ (t => Command(t))

  /**
    * A valueToken2 matches an uppercase letter followed by any number of non-space, non-bracket symbols
    *
    * @return a Parser[String]
    */
  val valueToken2: Parser[String] = """\p{Lu}[^\[\]\s]*""".r

  /**
    * A valueToken1 matches at least one non-space, non-bracket symbol
    *
    * @return a Parser[String]
    */
  val valueToken1: Parser[String] = """[^\[\]\s]+""".r

  private val openBracket = """\[""".r
  private val closeBracket = """]""".r

}

abstract class ArgsException(s: String) extends Exception(s"Args exception: $s")

case object AnonymousNotFoundException extends ArgsException("no anonymous arg found")

case class NotFoundException(command: String) extends ArgsException(s"Arg: command $command not found")

case class AmbiguousNameException(name: String) extends ArgsException(s"$name ambiguous")

case class ParseException(cause: String) extends ArgsException(cause)

case class NoValueException(name: Option[String]) extends ArgsException(s"Arg: command ${name.getOrElse("anonymous")} has no value")

case class ValidationException[X](a: Args[X], s: Synopsis) extends ArgsException(s"Args: validation failed for $a with synopsis: $s")

case class InvalidOptionException[X](arg: Arg[X]) extends ArgsException(s"Arg ${arg.name} not valid")

case class CompareException(str: String) extends ArgsException(s"Arg compare exception: $str")
