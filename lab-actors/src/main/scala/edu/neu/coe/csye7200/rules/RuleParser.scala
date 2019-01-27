/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import scala.collection.mutable
import scala.language.implicitConversions
import scala.util.Try
import scala.util.parsing.combinator.JavaTokenParsers

/**
  * RuleParser
  *
  * This module/package does more than just parse Rules. In particular, it has a facility to parse expressions too.
  *
  * NOTE: the parsing and toString of this package are not compatible so that Json cannot currently be used.
  */

/**
  * Sealed trait RuleLike which describes the result of parsing a rule.
  * In this context, a rule is defined by several different case classes, each defining
  * a different method of combining rules together into a tree of rules.
  * In general, a leaf rule is a Condition and is made up of two features: a subject and a predicate.
  */
sealed trait RuleLike {
  /**
    * Method to convert this (parsed) RuleLike instance into a Rule[String]
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String]
}

case class Conjunction(fs: List[RuleLike]) extends RuleLike {
  /**
    * Method to convert this Conjunction into a conjunction of Rules
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String] = fs match {
    case Nil => InvalidRule(new PredicateException("empty Conjunction"))
    case h :: Nil => h.asRule
    case _ => fs.foldLeft[Rule[String]](Truth(true))((a, b) => a :& b.asRule)
  }
}

case class Disjunction(ts: List[RuleLike]) extends RuleLike {
  /**
    * Method to convert this Disjunction into a disjunction of Rules
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String] = ts match {
    case Nil => InvalidRule(new PredicateException("empty Disjunction"))
    case h :: Nil => h.asRule
    case _ => ts.foldLeft[Rule[String]](Truth(false))((a, b) => a :| b.asRule)
  }
}

case class Parentheses(rule: RuleLike) extends RuleLike {
  /**
    * Method to convert this parenthetical RuleLike into a Rule
    *
    * @return a Rule[String] that corresponds to this (parsed) RuleLike
    */
  def asRule: Rule[String] = rule.asRule
}

/**
  * Condition: defines a leaf RuleLike instance.
  *
  * @param subject   the subject of this condition
  * @param predicate the predicate of this condition
  */
case class Condition(subject: String, predicate: PredicateExpr) extends RuleLike {
  // NOTE that this involves an implicit conversion from PredicateExpr to Predicate
  val p: Predicate[String] = predicate match {
    case e: RangePredicateExpr => e
    case b: BooleanPredicateExpr => b
  }

  def asRule: Rule[String] = new BoundPredicate[String](subject, p)
}

/**
  * Truth value: always or never true
  *
  * @param b the truth value
  */
case class TruthValue(b: Boolean) extends RuleLike {
  def asRule: Rule[String] = Truth(b)

  // XXX: this is somewhat experimental
  override def toString: String = if (b) "always" else "never"
}

sealed trait PredicateExpr

/**
  * a predicate expression consisting of an operator and an operand.
  *
  * @param operator the operator. for example "<", ">=", etc.
  * @param operand  the operand, i.e. the RHS of the operator.
  */
case class BooleanPredicateExpr(operator: String, operand: Expression) extends PredicateExpr

/**
  * a predicate expression consisting of an two bounds
  *
  * @param operand1 the lower bound
  * @param operand2 the upper bound
  */
case class RangePredicateExpr(operand1: Expression, operand2: Expression) extends PredicateExpr

/**
  * RuleParser is a parser-combinator which parses rules. [Wow, that's a surprise!]
  * Of course, it can also parse any of the constituents of a rule (expression, number, predicate, etc.).
  *
  * A RuleLike is defined as a sealed trait which is extended by several different case classes, each defining
  * a different method of combining rules together into a tree of rules.
  * In general, a leaf rule is a Condition and is made up of two features: a subject and a predicate.
  *
  * It's important to note that all aspects of these rules are String-based. There is absolutely no conversion to numeric
  * types anywhere in this class.
  *
  * The trait RuleLike has only one method: asRule which returns a Rule[String]
  *
  * Created by scalaprof on 5/30/16.
  */
class RuleParser extends JavaTokenParsers {

  def parseRule(s: String): Try[RuleLike] = {
    parseAll(rule, s) match {
      case this.Success(p, _) => scala.util.Success(p)
      case this.Failure(x, _) => RuleParser.parseFailure(s, "rule", x)
      case this.Error(x, _) => RuleParser.parseFailure(s, "rule", x)
    }
  }

  def parseExpression(s: String): Try[Expression] = {
    parseAll(expr, s) match {
      case this.Success(p, _) => scala.util.Success(p)
      case this.Failure(x, _) => RuleParser.parseFailure(s, "expression", x)
      case this.Error(x, _) => RuleParser.parseFailure(s, "expression", x)
    }
  }

  def rule: Parser[RuleLike] = repsep(term, alternation) ^^ (ts => Disjunction(ts))

  def alternation: Parser[String] = "|" | "(?i)or".r

  def term: Parser[RuleLike] = repsep(factor, ampersand) ^^ (fs => Conjunction(fs))

  def ampersand: Parser[String] = "&" | "(?i)and".r

  def factor: Parser[RuleLike] = (always | never | condition | "(" ~> rule <~ ")" | failure("problem with factor")) ^^ {
    case t: TruthValue => t
    case c: Condition => c
    case r: RuleLike => Parentheses(r)
  }

  def always: Parser[RuleLike] = """(?i)always|true""".r ^^ (_ => TruthValue(true))

  def never: Parser[RuleLike] = """(?i)never|false""".r ^^ (_ => TruthValue(false))

  def condition: Parser[RuleLike] = identifier ~ predicate ^^ { case s ~ p => Condition(s, p) }

  def predicate: Parser[PredicateExpr] = booleanPredicate | rangePredicate

  def booleanPredicate: Parser[BooleanPredicateExpr] = booleanOp ~ expr ^^ { case o ~ v => BooleanPredicateExpr(o, v) }

  def rangePredicate: Parser[RangePredicateExpr] = "in" ~ expr ~ "..." ~ expr ^^ { case _ ~ l ~ _ ~ h => RangePredicateExpr(l, h) }

  private val booleanOp = regex(""">|>=|<|<=|=|!=""".r)
  // CONSIDER implement this...
  //  private val lesserOp = regex("""<|<=""".r)
  private val identifier = regex("""\w+""".r)
  private val identifierWithPeriods = regex("""[\w\.]+""".r)
  private val doubleQuote = """"""".r

  abstract class ExprFactor extends Expression

  case class Expr(t: ExprTerm, ts: List[String ~ ExprTerm]) extends Expression {
    //noinspection ScalaDeprecation
    def toRPN: List[String] = {
      val stack = new mutable.Stack[String]()

      def shunt(xs: List[String], et: String ~ ExprTerm): List[String] = et match {
        case op ~ x => stack.push(op); xs ++ x.toRPN
      }

      val rpn: List[String] = ts.foldLeft(t.toRPN)(shunt)
      rpn ++ stack.elems.reverse
    }

    def asString: String = ts.foldLeft(t.toString)(_ + _.toString)

    def asQuotedString: Option[String] = if (ts.isEmpty) t.asQuotedString else None
  }

  case class ExprTerm(f: ExprFactor, fs: List[String ~ ExprFactor]) extends Expression {
    //noinspection ScalaDeprecation
    def toRPN: List[String] = {
      val stack = new mutable.Stack[String]()

      def shunt(xs: List[String], et: String ~ ExprFactor): List[String] = et match {
        case op ~ x => stack.push(op); xs ++ x.toRPN
      }

      val rpn: List[String] = fs.foldLeft(f.toRPN)(shunt)
      rpn ++ stack.elems.reverse
    }

    def asString: String = fs.foldLeft(f.toString)(_ + _.toString)

    def asQuotedString: Option[String] = if (fs.isEmpty) f.asQuotedString else None
  }

  /**
    * a Number with a suffix multiplier
    *
    * @param n the number in string form
    * @param m a multiplier such as B, M, K or 1 for multiples of 1000000000, 1000000, 1000, or 1 respectively.
    */
  case class Number(n: String, m: String) extends ExprFactor {
    def expand(s: String): List[String] = s match {
      case "B" => expand("K") ++ expand("M")
      case "M" => expand("K") ++ expand("K")
      case "K" => List("1000", "*")
      case "1" => List()
      case "%" => List("100", "/")
      case _ => throw new RuleException("Number exprFactor must be B, M, K, %, or 1")
    }

    def asString: String = s"$n*${expand(m)}"

    def toRPN: List[String] = n +: expand(m)

    def asQuotedString: Option[String] = None

    //    override def toString = asString
  }

  case class Variable(x: String) extends ExprFactor {
    def asString: String = "$" + s"$x"

    def toRPN: List[String] = List(asString)

    def asQuotedString: Option[String] = None
  }

  case class Literal(s: String) extends ExprFactor {
    def asString: String = s""""$s""""

    def toRPN: List[String] = List(asString)

    def asQuotedString: Option[String] = Some(s)
  }

  /**
    * XXX this appears not to be tested
    *
    * @param e the expression inside the parentheses
    */
  case class ExprParentheses(e: Expr) extends ExprFactor {
    def toRPN: List[String] = e.toRPN

    def asString: String = s"($e)"

    def asQuotedString: Option[String] = None
  }

  //  /**
  //    *
  //    * @param s the String which defines this expression
  //    */
  //  case class ExprValue(s: String) extends Expression {
  //    def toRPN = List("$" + s)
  //
  //    def asString: String = s"($s)"
  //  }

  def expr: Parser[Expression] = exprTerm ~ rep("+" ~ exprTerm | "-" ~ exprTerm | failure("expr")) ^^ {
    case t ~ r => r match {
      case x: List[String ~ ExprTerm] => Expr(t, x)
    }
  }

  def exprTerm: Parser[ExprTerm] = exprFactor ~ rep("*" ~ exprFactor | "/" ~ exprFactor | failure("exprTerm")) ^^ {
    case f ~ r => r match {
      case x: List[String ~ Expression] => ExprTerm(f, x)
    }
  }

  def exprFactor: Parser[ExprFactor] = (qualifiedNumber | quotedString | lookup | "(" ~ expr ~ ")" | failure("exprFactor")) ^^ {
    case "(" ~ e ~ ")" => e match {
      case x: Expr => ExprParentheses(x)
    }
    case s: String => Variable(s)
    case n: Number => n
    case q: Literal => q
  }

  def qualifiedNumber: Parser[Number] = number ~ opt(suffix) ^^ {
    case n ~ Some(p) => Number(n, p)
    case n ~ None => Number(n, "1")
  }

  def number: Parser[String] = floatingPointNumber | wholeNumber | failure("problem with number")

  def suffix: Parser[String] = ("""[BMK%]""".r | """@""".r ~ identifier | failure("problem with suffix")) ^^ { case _ ~ id => id.toString; case s => s.toString; }

  // CONSIDER: this works for variables on RHS, but we can't have something like {x.y} on the LHS of the condition. We could allow for that
  def lookup: Parser[String] = ("""${""" ~ identifierWithPeriods <~ """}""" | "$" ~ identifier) ^^ { case _ ~ x => x }

  def quotedString: Parser[Literal] = doubleQuote ~> """[^"]*""".r <~ doubleQuote ^^ { s => Literal(s) }
}

object RuleParser {
  def parseFailure[X](s: String, e: String, x: String): Try[X] = {
    scala.util.Failure(new PredicateException(s"""unable to parse "$s" as a $e: $x"""))
  }
}
