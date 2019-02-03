/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.rules

import edu.neu.coe.csye7200.rules.FP._

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * This trait extends the function class T=>Try[Boolean].
  * Predicates are intended to combine and form rules.
  * At present, a Predicate is a Functor but not a Monad.
  *
  * Created by robinhillyard on 5/24/16.
  */
trait Predicate[T] extends (T => Try[Boolean]) {
  self =>

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * If f(t) throws an exception, then map will throw that exception.
    * If you want a more pure functional programming map function, then use tryMap.
    *
    * @param f the map function, a T=>U
    * @tparam U the underlying type of the resulting Predicate
    * @return a Predicate[U]
    */
  def map[U: Ordering](f: T => U): Predicate[U] = tryMap((t: T) => Try(f(t))).get


  /**
    * The tryMap function for Predicate. This is something like flatMap, although strictly speaking, not the same.
    * As with map, note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: T => Try[U]): Try[Predicate[U]]

  def asFunction: T => Try[Boolean] = self

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if p evaluates as false
    *
    * @param p the other Predicate
    * @return a Predicate which is the conjunctive (and) combination of p with self
    */
  def &:(p: T => Try[Boolean]) = And(Predicate(p), self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if p evaluates as false
    *
    * @param p the other Predicate
    * @return a Predicate which is the conjunctive (and) combination of p with self
    */
  def &:(p: Predicate[T]) = And(Predicate(p), self)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if p evaluates as true
    *
    * @param p the other Predicate
    * @return a Predicate which is the disjunctive (or) combination of p with self
    */
  def |:(p: T => Try[Boolean]) = Or(Predicate(p), self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of self with p
    */
  def :&(p: T => Try[Boolean]) = And(self, Predicate(p))

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param p the other Predicate (p will not be evaluated if self evaluates as true)
    * @return a Predicate which is the disjunctive (or) combination of self with p
    */
  def :|(p: T => Try[Boolean]) = Or(self, Predicate(p))

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if f evaluates as false
    *
    * @param f the other function T=>Boolean (f will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of f with self
    */
  def &^:(f: T => Boolean) = And(Func(f), self)

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the right.
    *
    * Self will not be evaluated if f evaluates as true
    *
    * @param f a T=>Boolean function
    * @return a Predicate which is the disjunctive (or) combination of p with self
    */
  def |^:(f: T => Boolean) = Or(Func(f), self)

  /**
    * Conjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param f the other Predicate (p will not be evaluated if self evaluates as false)
    * @return a Predicate which is the conjunctive (and) combination of self with p
    */
  def :^&(f: T => Boolean) = And(self, Func(f))

  /**
    * Disjunctive combination of self with another Predicate such that
    * the inputs to both predicates are the same single parameter.
    *
    * Associates to the left.
    *
    * @param f the other function T=>Boolean (f will not be evaluated if self evaluates as true)
    * @return a Predicate which is the disjunctive (or) combination of self with f
    */
  def :^|(f: T => Boolean) = Or(self, Func(f))
}

/**
  * abstract class which partially implements Predicate[T]
  *
  * @param name the name of this predicate
  * @tparam T the underlying type of the Predicate
  */
abstract class BasePredicate[T](name: String) extends Predicate[T] {
  self =>
  override def toString: String = name
}

/**
  * "And" sub-class of BasePredicate yielding the conjunction of p1 and p2.
  *
  * @param p1 a function T=>Try[Boolean] which will typically be a Predicate; this function will always be evaluated
  * @param p2 a function T=>Try[Boolean] which will typically be a Predicate; this function may not be evaluated
  * @tparam T the underlying type of the Predicate
  */
case class And[T](p1: Predicate[T], p2: Predicate[T]) extends BasePredicate[T](s"($p1)&($p2)") {
  // TODO make map2 lazy, as in LaScala
  def apply(t: T): Try[Boolean] = map2(p1(t), p2(t))(_ && _)


  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (q1 <- p1 tryMap f; q2 <- p2 tryMap f) yield And(q1, q2)
}

/**
  * "Or" sub-class of BasePredicate yielding the disjunction of p1 and p2.
  *
  * @param p1 a function T=>Try[Boolean] which will typically be a Predicate; this function will always be evaluated
  * @param p2 a function T=>Try[Boolean] which will typically be a Predicate; this function may not be evaluated
  * @tparam T the underlying type of the Predicate
  */
case class Or[T](p1: Predicate[T], p2: Predicate[T]) extends BasePredicate[T](s"($p1)&($p2)") {
  def apply(t: T): Try[Boolean] = map2(p1(t), p2(t))(_ || _)

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (q1 <- p1 tryMap f; q2 <- p2 tryMap f) yield Or(q1, q2)

}

/**
  * "GT" sub-class of BasePredicate which evaluates to true if x > y where x is the parameter passed into the apply method.
  *
  * @param y a T expression
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class GT[T: Ordering](y: T) extends BasePredicate[T](s">$y") {
  self =>
  def apply(x: T) = Try(implicitly[Ordering[T]].gt(x, y))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (u <- f(y)) yield GT(u)
}

/**
  * "LT" sub-class of BasePredicate which evaluates to true if x < y where x is the parameter passed into the apply method.
  *
  * @param y a T expression
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class LT[T: Ordering](y: T) extends BasePredicate[T](s"<$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].lt(x, y))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (u <- f(y)) yield LT(u)
}

/**
  * "GE" sub-class of BasePredicate which evaluates to true if x >= y where x is the parameter passed into the apply method.
  *
  * @param y a T expression
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class GE[T: Ordering](y: T) extends BasePredicate[T](s">=$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].gteq(x, y))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (u <- f(y)) yield GE(u)
}

/**
  * "LE" sub-class of BasePredicate which evaluates to true if x <= y where x is the parameter passed into the apply method.
  *
  * @param y a T expression
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class LE[T: Ordering](y: T) extends BasePredicate[T](s"<=$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].lteq(x, y))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (u <- f(y)) yield LE(u)
}

/**
  * "EQ" sub-class of BasePredicate which evaluates to true if x = y where x is the parameter passed into the apply method.
  *
  * @param y a T expression
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class EQ[T: Ordering](y: T) extends BasePredicate[T](s"=$y") {
  def apply(x: T) = Try(implicitly[Ordering[T]].equiv(x, y))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (u <- f(y)) yield EQ(u)
}

/**
  * "NE" sub-class of BasePredicate which evaluates to true if x != y where x is the parameter passed into the apply method.
  *
  * @param y a T expression
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class NE[T: Ordering](y: T) extends BasePredicate[T](s"!=$y") {
  def apply(x: T) = Try(!implicitly[Ordering[T]].equiv(x, y))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (u <- f(y)) yield NE(u)
}

/**
  * "Func" sub-class of BasePredicate which evaluates to true if p(x) where x is the parameter passed into the apply method.
  *
  * @param p a T=> Boolean function (might be another predicate)
  * @tparam T the underlying type of the Predicate, which must implement Ordering
  */
case class Func[T](p: T => Boolean) extends BasePredicate[T](s"function $p") {
  def apply(x: T) = Try(p(x))

  /**
    * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = throw new PredicateException("NYI: Func.tryMap")
}

/**
  * "Pred" sub-class of BasePredicate which evaluates to true if p(f(x)) where x is the parameter passed into the apply method.
  *
  * @param p a Predicate
  * @param f a T=>V function
  * @tparam T the input type of the function f
  * @tparam V the output type of the function f and the underlying type of the resulting Predicate
  */
case class Pred[T, V](p: Predicate[V])(f: T => V) extends BasePredicate[T](s"$p with $f") {
  def apply(x: T): Try[Boolean] = p(f(x))

  /**
    *
    * @param g a function which transforms a T into a Try[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](g: (T) => Try[U]): Try[Predicate[U]] = Failure(new PredicateException("NYI: Pred.tryMap"))
}

/**
  * "In" sub-class of BasePredicate which evaluates to true if as contains x where x is the parameter passed into the apply method.
  *
  * @param as a sequence of A values
  * @tparam A the underlying type of the sequence
  */
case class In[A](as: List[A]) extends BasePredicate[A](s"in $as...") {
  def apply(x: A) = Try(as.contains(x))

  /**
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (A) => Try[U]): Try[Predicate[U]] = Failure(new PredicateException("NYI: In.tryMap"))
}

/**
  * "Matches" sub-class of BasePredicate which evaluates to true if f.isDefinedAt(x) and f(x) where x is the parameter passed into the apply method.
  *
  * @param f a T => Boolean partial function
  * @tparam T the underlying type of the Predicate
  */
case class Matches[T](f: PartialFunction[T, Boolean]) extends BasePredicate[T](s"matches $f") {
  def apply(x: T) = Try(f.isDefinedAt(x) && f(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    * @tparam U the underlying type of the resulting Predicate
    * @return a Predicate[U]
    */
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = Failure(new PredicateException("NYI: Matches.tryMap"))
}

/**
  * "InRange" sub-class of BasePredicate[Int] which evaluates to true if r contains x where x is the parameter passed into the apply method.
  *
  * @param r a Range
  */
case class InRange(r: Range) extends BasePredicate[Int](s"in $r") {
  def apply(x: Int) = Try(r.contains(x))

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    * @tparam U the underlying type of the resulting Predicate
    * @return a Predicate[U]
    */
  def tryMap[U: Ordering](f: (Int) => Try[U]): Try[Predicate[U]] = Failure(new PredicateException("NYI: InRange.tryMap"))
}

/**
  * "InBounds" sub-class of BasePredicate which evaluates to true if x >= min & x <= max where x is the parameter passed into the apply method.
  *
  * @param min a T expression
  * @param max a T expression
  * @tparam T the type of both min and max, and also the underlying type of the resulting Predicate
  */
case class InBounds[T: Ordering](min: T, max: T) extends BasePredicate[T](s"in bounds $min..$max") {
  def apply(x: T): Try[Boolean] = (GE(min) :& LE(max)) (x)

  /**
    * The map function for Predicate. Note that for operators such as >, <=, etc. it is essential
    * that the mapping between T and U be isomorphic.
    *
    * This method will fail where the predicate does not rely on Ordering.
    *
    * @param f the map function, an T=>U
    * @tparam U the underlying type of the resulting Predicate
    * @return a Predicate[U]
    */
  def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = for (q1 <- f(min); q2 <- f(max)) yield InBounds(q1, q2)
}

/**
  * "Always" sub-class of BasePredicate which evaluates to true.
  * //@tparam T
  */
case object Always extends BasePredicate[Any]("true") {
  /**
    *
    * @param t a parameter of type Any
    * @return Success(true)
    */
  def apply(t: Any): Try[Boolean] = Success(true)

  /**
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (Any) => Try[U]): Try[Predicate[U]] = Success(new BasePredicate[U]("Always mapped") {
    /**
      * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
      * that the mapping between T and U be isomorphic.
      *
      * This method will fail where the predicate does not rely on Ordering.
      *
      * @param f the map function, an T=>Predicate[U]
      * @tparam V the underlying type of the resulting Predicate
      * @return a Try[Predicate[U]
      **/
    def tryMap[V: Ordering](f: (U) => Try[V]): Try[Predicate[V]] = Failure(new PredicateException("NYI: apply.tryMap"))

    /**
      *
      * @param v1 a parameter of type U
      * @return Success(true)
      */
    def apply(v1: U): Try[Boolean] = Success(true)
  })
}

/**
  * "Never" sub-class of BasePredicate which evaluates to false.
  * //@tparam T
  */
case object Never extends BasePredicate[Any]("false") {
  def apply(t: Any): Try[Boolean] = Success(false)

  /**
    *
    * @param f the map function, an T=>Predicate[U]
    * @tparam U the underlying type of the resulting Predicate
    * @return a Try[Predicate[U]
    **/
  def tryMap[U: Ordering](f: (Any) => Try[U]): Try[Predicate[U]] = Success(new BasePredicate[U]("Always mapped") {
    /**
      * The tryMap function for Predicate. Note that for operators such as >, <=, etc. it is essential
      * that the mapping between T and U be isomorphic.
      *
      * This method will fail where the predicate does not rely on Ordering.
      *
      * @param f the map function, an T=>Predicate[U]
      * @tparam V the underlying type of the resulting Predicate
      * @return a Try[Predicate[U]
      **/
    def tryMap[V: Ordering](f: (U) => Try[V]): Try[Predicate[V]] = Failure(new PredicateException("NYI: apply.tryMap"))

    def apply(v1: U): Try[Boolean] = Success(false)
  })
}

case class InvalidPredicate(x: Throwable) extends BasePredicate[Any](s"invalid: $x") {
  def apply(t: Any): Try[Boolean] = Failure(x)

  def tryMap[U: Ordering](f: (Any) => Try[U]): Try[Predicate[U]] = Success(InvalidPredicate(x).asInstanceOf[Predicate[U]])
}

class PredicateException(s: String) extends Exception(s"rule problem: $s")

object Predicate {
  /**
    * Given a predicate function p which transforms a T into a Try[Boolean], construct a new Predicate[T]
    *
    * @param p the predicate function
    * @tparam T the underlying type of the predicate function and the resulting Predicate
    * @return a new BasePredicate
    */
  def apply[T](p: T => Try[Boolean]): Predicate[T] = new BasePredicate[T](s"$p") {
    self =>
    def tryMap[U: Ordering](f: (T) => Try[U]): Try[Predicate[U]] = Failure(new PredicateException("NYI apply.map"))

    def apply(t: T): Try[Boolean] = p(t)
  }

  implicit def convertFromRangePredicateExpr(x: RangePredicateExpr): Predicate[String] = {
    // NOTE for now, we will just turn an RPN list into a String
    val p1: String = x.operand1.toRPN.mkString(" ")
    val p2: String = x.operand2.toRPN.mkString(" ")
    InBounds(p1, p2)
  }

  implicit def convertFromBooleanPredicateExpr(x: BooleanPredicateExpr): Predicate[String] = {
    // NOTE for now, we will just turn an RPN list into a String
    val p: String = x.operand.toRPN.mkString(" ")
    getPredicate(x, p)
  }

  private def getPredicate[T: Ordering](x: BooleanPredicateExpr, p: T): Predicate[T] = {
    x.operator match {
      case ">" => GT(p)
      case ">=" => GE(p)
      case "<" => LT(p)
      case "<=" => LE(p)
      case "=" => EQ(p)
      case "!=" => NE(p)
      case _ => throw new PredicateException(s"NYI: $x")
    }
  }
}
