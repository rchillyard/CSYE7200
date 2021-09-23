/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.parse

import com.phasmid.laScala.values.{QuotedStringScalar, Scalar}
import com.phasmid.laScala.{Prefix, Renderable}
import edu.neu.coe.csye7200.util.Logger

import scala.util._

/**
  * Trait to represent function invocation as parsed using the FunctionParser
  *
  * Created by scalaprof on 1/11/17.
  *
  */
sealed trait Invocation extends Renderable {

  def name: String

  def xs: Seq[Expression]

  //  /**
  //    * Method to yield an InvocationClosure for this Invocation
  //    *
  //    * @param functionLibrary the function library wherein names will be looked up
  //    * @tparam T the underlying parameter type
  //    * @tparam R the underlying result type
  //    * @return an InvocationClosure wrapped in Try
  //    */
  //  def asClosure[T: ClassTag, R: ClassTag](implicit functionLibrary: FunctionLibrary): Try[InvocationClosure[R]]

  /**
    * Method to determine the required arity for this Invocation.
    * Subclasses should override this if they require a non-zero arity.
    *
    * NOTE: this method does NOT give the effective arity of this Invocation. It returns the REQUIRED arity.
    *
    * @return the required arity (defaults to 0)
    */
  def arity: Int = 0

  //  /**
  //    * Method to validate this Invocation. For now, it checks the arity.
  //    *
  //    * @param functionLibrary (implicit)
  //    * @return
  //    */
  //  def validate(implicit functionLibrary: FunctionLibrary): Boolean = Invocation.validateThis(this) || validateChildren

  //  /**
  //    * Method to validate this Invocation. For now, it checks the arity.
  //    *
  //    * @param functionLibrary (implicit)
  //    * @return true if the children are valid
  //    */
  //  def validateChildren(implicit functionLibrary: FunctionLibrary): Boolean

  /**
    * Method to add the given parameter s to the head of the list of expressions of this invocation
    *
    * @param x the Expression to add
    */
  //noinspection ScalaStyle
  def +:(x: Expression): Invocation
}

/**
  * Base class for Invocation
  *
  * @param n  the name
  * @param es the parameters
  */
abstract class InvocationBase(n: String, es: List[Expression]) extends Invocation with Serializable {

  def name: String = n

  def xs: List[Expression] = es

  //  def asClosure[T: ClassTag, R: ClassTag](implicit functionLibrary: FunctionLibrary): Try[InvocationClosure[R]] =
  //    for (f <- functionLibrary.asTry[R](name); c: InvocationClosure[R] = InvocationClosureFunction(f, es)) yield c

  /**
    * This default implementation throws an exception.
    * It should be overridden where appropriate.
    *
    * @param s the Scalar to add
    */
  //noinspection ScalaStyle
  def +:(s: Expression): Invocation = throw ParserException(s"Logic error: cannot add $s to $this") // NOTE: This should never be invoked

  //  def validateChildren(implicit functionLibrary: FunctionLibrary): Boolean = (for (e <- es) yield e match {
  //    case i: Invocation => i.validate
  //    case _ => true
  //  }).forall(b => b)

  import Renderable.renderableTraversable

  def render(indent: Int)(implicit tab: Int => Prefix): String = name + es.render(indent + 1)

  /**
    * If you want the more compact form of String, then just comment this override out.
    *
    * @return
    */
  override def toString: String = render(0)
}

object InvocationBase {
  /**
    * Extractor method.
    *
    * @param arg an Invocation to be extracted
    * @return an Option of Tuple2 corresponding to the original parameters
    */
  def unapply(arg: Invocation): Option[(String, Seq[Expression])] = Some((arg.name, arg.xs))

  def renderExpression(e: Expression, indent: Int)(implicit tab: Int => Prefix): String = e match {
    case Left(l) => l.render(indent + 1)
    case Right(r) => s"(\n${tab(indent + 1)}" + r.render(indent + 1) + ")"
  }

  /**
    * This method provides an alternative to an if... in a for-comprehension involving Try objects.
    * The advantage is that the failure case is predictable and pattern-matchable (unlike NoSuchElementException) which is what you get from an if... that fails
    *
    * @param b  the condition to be tested
    * @param xt the result when the condition is true
    * @param s  the message which will be passed to the exception in the failing case
    * @tparam X the underlying type of the result
    * @return a Try[X]
    */
  def test[X](b: => Boolean, xt: => Try[X], s: => String): Try[X] = if (b) xt else Failure(TestFailed(s)) // CHECK
}

/**
  * This class provides an alternative to NoSuchElementException which arises from an "if..." in a for-comprehension
  *
  * @param s the message
  */
case class TestFailed(s: String) extends Exception(s"predicate failed: $s")

/**
  * Class to represent function invocation in the form (p1,p2,...,pN) as parsed using the FunctionParser and where the list should be
  * treated as a single parameter to another invocation.
  *
  * @param xs the list of Expressions which were parsed: that's to say the parameters of the function.
  *           An Expression is either a Scalar or a (nested) invocation.
  */
case class InvocationPn1(override val xs: List[Expression]) extends InvocationBase("varargs", xs) {
  //  override def asClosure[T: ClassTag, R: ClassTag](implicit functionLibrary: FunctionLibrary): Try[InvocationClosure[R]] = {
  //    val invocationClosureVarArgs: InvocationClosure[R] = InvocationClosureVarArgs[Seq[T]](xs).asInstanceOf[InvocationClosure[R]]
  //    Try(invocationClosureVarArgs)
  //  }

  override def +:(x: Expression): Invocation = InvocationPn1(x +: xs) // CHECK
}

/**
  * Class to represent function invocation in the form f(p1,p2,...,pN) as parsed using the FunctionParser
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param name the name of the invocation -- this is the identifier that precedes the open parenthesis
  * @param xs   the list of Expressions which were parsed: that's to say the parameters of the function.
  *             An Expression is either a Scalar or a (nested) invocation
  */
case class InvocationFPn(override val name: String, override val xs: List[Expression]) extends InvocationBase(name, xs) {
  override def +:(x: Expression): Invocation = InvocationFPn(name, x +: xs)
}

/**
  * Class to represent function invocation in the form (p) as parsed using the FunctionParser
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param x the Expression which was parsed
  */
case class InvocationP(x: Expression) extends InvocationBase("identity", List(x)) {
  override def +:(x: Expression): Invocation = InvocationFPn(name, x +: xs) // CHECK
}

/**
  * Class to represent function invocation in the form p f as parsed using the FunctionParser
  * Example: DATE(2017,2,23)
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param x the expression which precedes the function
  * @param f the function (an Invocation)
  */
case class InvocationPF(x: Expression, f: Invocation) extends InvocationBase("InvocationPF", Nil) {

  //  override def asClosure[T: ClassTag, R: ClassTag](implicit functionLibrary: FunctionLibrary): Try[InvocationClosure[R]] = f match {
  //    case InvocationFP(n, Right(i)) => InvocationFP(n, Right(i.+:(x))).asClosure(implicitly[ClassTag[T]], implicitly[ClassTag[R]], functionLibrary)
  //    case InvocationFP(n, z) => InvocationFPn(n, List(x, z)).asClosure(implicitly[ClassTag[T]], implicitly[ClassTag[R]], functionLibrary)
  //    case InvocationFPn(n, xs) => InvocationFPn(n, x +: xs).asClosure(implicitly[ClassTag[T]], implicitly[ClassTag[R]], functionLibrary)
  //    case _ => throw ParserException(s"Logic error: asClosure not implemented for InvocationPF($x,$f)") // CHECK
  //  }

  override def render(indent: Int = 0)(implicit tab: Int => Prefix): String =
    name + "(\n" + tab(indent + 1) + x.render(indent + 1) + " " + f.render(indent + 1) + ")"
}

/**
  * Class to represent function invocation in the form f p as parsed using the FunctionParser
  * Example: NOT TRUE
  *
  * Created by scalaprof on 1/11/17.
  *
  * @param name the name of the invocation -- this is the identifier that precedes the open parenthesis
  * @param p    the Expression which follows the name
  */
case class InvocationFP(override val name: String, p: Expression) extends InvocationBase(name, List(p)) {
  override def +:(x: Expression): Invocation = InvocationFPn(name, x +: List(p)) // CHECK
}

case class InvocationLookup(s: String) extends InvocationBase("lookup", List(Left(Scalar(s))))

/**
  * Class to represent WhenThen invocation in the form WHEN f THEN g as parsed using the FunctionParser
  *
  * Created by scalaprof on 3/2/17.
  *
  */
case class InvocationCaseClause(whenThens: List[Invocation], elseClause: Option[Expression]) extends
  InvocationBase("case", (whenThens map (Right(_))) ++ elseClause.toList) {
  //  private def accumulate(a: Invocation, b: Invocation)(implicit functionLibrary: FunctionLibrary): Invocation = InvocationGetOrElse(a, b)

  //  override def asClosure[T: ClassTag, R: ClassTag](implicit functionLibrary: FunctionLibrary): Try[InvocationClosure[R]] = {
  //    val terminator = elseClause map (InvocationWhenThen(InvocationP(Left(Scalar(true))), _))
  //    val invocation = (whenThens ++ terminator.toList).foldLeft[Invocation](InvocationWhenThen(InvocationP(Left(Scalar(false))),
  //      Right(InvocationP(Left(Scalar(0))))))(accumulate)
  //    invocation.asClosure(implicitly[ClassTag[T]], implicitly[ClassTag[R]], functionLibrary)
  //  }
}

/**
  * Class to represent WhenThen invocation in the form WHEN f THEN g as parsed using the FunctionParser
  *
  * The function which implements whenThen takes a Boolean and an Any; and returns an Any which is possibly null
  *
  * Created by scalaprof on 3/2/17.
  *
  */
case class InvocationWhenThen(when: Invocation, `then`: Expression) extends InvocationBase("whenThen", List(Right(when), `then`)) // CHECK

/**
  * Class to represents an Invocation which evaluates the x invocation and, if that results in a non-null result, will yield that result.
  * If, on the other hand, the result of x is null, then we evaluate y.
  *
  * @param x the Invocation which embodies all of the WhenThen clauses that we have already addressed
  * @param y an InvocationWhenThen
  */
case class InvocationGetOrElse(x: Invocation, y: Invocation) extends InvocationBase("getOrElse", List(Right(x), Right(y))) // CHECK

/**
  * Class to represent WhenThen invocation in the form WHEN f THEN g as parsed using the FunctionParser
  *
  * Created by scalaprof on 3/2/17.
  *
  */
case class InvocationBooleanExpression(first: Expression, terms: List[BooleanTerm]) extends InvocationBase("boolean", Nil) {

  //  private def accumulate(i: Invocation, t: BooleanTerm)(implicit functionLibrary: FunctionLibrary): Invocation =
  //    InvocationBoolean(t.op, Right(i), t.e)

  //  override def asClosure[T: ClassTag, R: ClassTag](implicit functionLibrary: FunctionLibrary): Try[InvocationClosure[R]] =
  //    terms.foldLeft[Invocation](InvocationP(first))(accumulate).asClosure(implicitly[ClassTag[T]], implicitly[ClassTag[R]], functionLibrary)

  override def render(indent: Int)(implicit tab: Int => Prefix): String =
    s"$name(${terms.foldLeft(first.render(indent + 1))(_ + " " + _.render(indent + 1))})"
}

case class InvocationBoolean(op: BooleanOp, x: Expression, y: Expression) extends InvocationBase(op.toString, List(x, y)) // CHECK

case class InvocationComparison(p: Expression, c: String, q: Expression) extends InvocationBase("compare", List(Left(InvocationComparison.asOp(c)), p, q))

// CHECK
case class InvocationColumn(i: Invocation, eo: Option[Scalar]) extends InvocationBase("column", List(Right(i)) ++ (eo.toList map (Left(_))))

// CHECK
case class InvocationSelect(is: List[Invocation], table: Scalar, wo: Option[Invocation], lo: Option[Scalar], oo: Option[Scalar]) extends
  InvocationBase("select", ((is map (Right(_))) :+ Left(table)) ++ (wo.toList map (Right(_))) ++ (lo.toList map (Left(_))) ++ (oo.toList map (Left(_))))

object InvocationComparison {
  val opMap = Map(">" -> "gt", ">=" -> "ge", "=" -> "eq", "<=" -> "le", "<" -> "lt", "<>" -> "ne", "!=" -> "ne", "LIKE" -> "like")

  def asOp(s: String): Scalar = QuotedStringScalar(opMap(s))
}

object Invocation {
  val logger: Logger = Logger(classOf[Invocation])

  //  private def validateThis(i: Invocation)(implicit functionLibrary: FunctionLibrary): Boolean = {
  //    val ok = (for (x: InvocationClosure[Any] <- i.asClosure[Any, Any]; y <- x.asClosure) yield i.arity == y.arity).getOrElse(false)
  //    if (!ok) logger.logWarning(s"Invocation.validate: $i is not valid (status of children to follow)")
  //    ok
  //  }


}
