package edu.neu.coe.csye7200.numerics

import org.apache.commons.math3.distribution._
import scala.annotation.tailrec
import scala.util._
import edu.neu.coe.csye7200.parse.FuzzyParser

/**
 * The Fuzzy trait defines a quantity for which there is doubt about its actual value.
 * This Fuzzy trait is Numeric (it extends Fractional) and thus Fuzzy values can be ordered,
 * compared, and take part in arithmetic operations, including division.
 * 
 * In this implementation of Fuzzy, the actual value is represented by a Double (see the get method).
 * 
 * In general, a Fuzzy trait would only require ordering.
 * 
 * Numeric fuzzy objects define a probability distribution such that it is possible to determine the
 * likelihood that the actual value of the fuzzy object is between two limits. Typical distributions
 * are Gaussian (i.e. "Normal") and Bounded (i.e. a truncated Uniform distribution). Other distributions
 * are possible of course, especially when Gaussian fuzzy objects are combined with other fuzzy objects.
 * This package does not properly handle these situations.
 * 
 * @author scalaprof
 */
sealed trait Fuzzy extends Fractional[Fuzzy] {
  /**
   * Method to map this Fuzzy object with a 1-arity DiFunc
	 * @param f the function to apply
	 * @return the result
	 */
  def map(f: DiFunc[Double]): Fuzzy
  /**
   * Method to map this Fuzzy object with a 2-arity DiFunc
	 * @param f the function to apply
	 * @param delta the fuzziness of the second parameter
	 * @return the result
	 */
  def map2(f: DiFunc[Double])(delta: Double): Fuzzy
  /**
	 * @return the nominal value of this Fuzzy object
	 */
  def get: Double
  /**
	 * @return some measure of this Fuzzy object's fuzziness (actual domain depends on type of Fuzzy)
	 */
  def fuzz: Double
  /**
	 * @return the underlying @link{RealDistribution} of this Fuzzy object
	 */
  def getRealDistribution: RealDistribution
  /**
   * Invokes the probability function of the @link{AbstractRealDistribution} but the parameters are slightly different.
   * 
	 * @param x the value at which we want to measure the probability
	 * @param delta the delta on either side which will determine the limits
	 * @return the probability that <code>x-delta <= x < x+delta</code>
	 */
  def prob(x: Double, delta: Double): Try[Double]
  
  /**
   * Returns an integer whose sign communicates how x compares to y.
   *
   * The result sign has the following meaning:
   *
   *  - negative if x < y
   *  - positive if x > y
   *  - 0 otherwise (if x == y or, for whatever reason, x cannot be distinguished from y)
   */
  def compare(x: Fuzzy, y: Fuzzy): Int = {
    val epsilon = 0.001
    val threshold = 0.6
    (x-y).prob(0,epsilon) match {
      case Success(p) => if (p>threshold) 0 else x.get.compare(y.get)
      case Failure(z) => System.err.println(s"exception thrown in prob method: $z"); 0
    }
  }
}

/**
 * Base class for the implementation of Fuzzy objects
 * 
 * @author scalaprof
 */
abstract class FuzzyBase(nominal: Double, delta: Double, distribution: AbstractRealDistribution) extends Fuzzy {  
  // Operators
  def + (that: Fuzzy): Fuzzy = plus(this,that)
  def + (that: Long): Fuzzy = this + Exact(that)
  def - (that: Fuzzy): Fuzzy = minus(this,that)
  def - (that: Long): Fuzzy = this - Exact(that)
  def unary_-: = negate(this)
  def * (that: Fuzzy): Fuzzy = times(this,that)
  def * (that: Long): Fuzzy = this * Exact(that)
  def / (that: Fuzzy): Fuzzy = this * Fuzzy.invert(that)
  def / (that: Long): Fuzzy = this / Exact(that)
  def ^ (that: Int): Fuzzy = power(that)
  
  // Members declared in scala.math.Numeric
  def fromInt(x: Int) = Exact(x)
  def minus(x: Fuzzy,y: Fuzzy): Fuzzy = plus(x,negate(y))
  def negate(x: Fuzzy): Fuzzy = x*Exact(-1)
  def plus(x: Fuzzy,y: Fuzzy): Fuzzy =  Fuzzy.sum(x,y)
  def times(x: Fuzzy,y: Fuzzy): Fuzzy = Fuzzy.product(x,y)
  def toDouble(x: Fuzzy): Double = x.get
  def toFloat(x: Fuzzy): Float = toFloat(x)
  def toInt(x: Fuzzy): Int = toInt(x)
  def toLong(x: Fuzzy): Long = toLong(x)
  
  //Members declared in scala.math.Fractional
  def div(x: Fuzzy,y: Fuzzy): Fuzzy = x/y
  
  // Type definition for Fuzzy
  type PairFunction = (Double,Double)=>Double

  // Other methods appropriate to Fuzzy
  def fuzz = delta
  def getRealDistribution: RealDistribution = distribution
  def prob(y: Double, delta: Double): Try[Double] = Try(distribution.probability(y-math.abs(delta), y+math.abs(delta)))
  def inverseCumulativeProbability(p: Double): Try[Double] = Try(distribution.inverseCumulativeProbability(p))
  /**
	 * @param o another Fuzzy which is to be added to this Fuzzy
	 * @param f a function(x,y) which 
	 * @param df_dx
	 * @param df_dy
	 * @return a new Fuzzy composed from this and o
 	*/
  def combine(o: Fuzzy, f: PairFunction, df_dx: PairFunction, df_dy: PairFunction): Fuzzy
  def negate = map(Negative)
  def get = nominal
  def map(f: DiFunc[Double]): Fuzzy = newFuzzy(f.f(nominal),math.abs(f.df_dx(0)(nominal)*delta))
  def map2(f: DiFunc[Double])(delta2: Double) = newFuzzy(f.f(nominal),math.abs(f.df_dx(0)(nominal)*delta)+math.abs(f.df_dx(1)(nominal)*delta2))
  def power(x: Int) = powerExact(this, x)
  def power(x: Double) = map(Power(x))
  def power(x: Fuzzy) = map2(Power(x.get))(x.fuzz)
  
  // Abstract factory method
  def newFuzzy(x: Double, delta: Double): Fuzzy
  
  def powerExact(x: Fuzzy, y: Int): Fuzzy = {
    @tailrec def inner(r: Fuzzy, left: Int): Fuzzy = if (left==0) r else inner(r*x, left-1)
    inner(Exact(1),y)
  }
}
case class Exact(x: Double) extends FuzzyBase(x,0,new ConstantRealDistribution(x)) {
  def combine(o: Fuzzy, f: PairFunction, df_dx: PairFunction, df_dy: PairFunction): Fuzzy = o match {
    case Exact(x2) => Exact(f(x,x2))
    case Gaussian(mu2,sigma2) => Gaussian(f(x,mu2),sigma2)
    case Bounded(mu2,delta2) => Bounded(f(x,mu2),delta2)
    case _ => throw new UnsupportedOperationException("NYI")
  }
  override def map(f: DiFunc[Double]) = Exact(f.f(x))
  override def map2(f: DiFunc[Double])(delta: Double) = throw new UnsupportedOperationException("cannot introduce fuzz to Exact")
  def newFuzzy(x: Double, delta: Double) = {require(delta==0); Exact(x)}
  override def toString = s"Exact: $x"
}
case class Gaussian(mu: Double, sigma: Double) extends FuzzyBase(mu,sigma,new NormalDistribution(mu.doubleValue,sigma.doubleValue)) {
  require(sigma>0,"sigma must be positive")
  def combine(o: Fuzzy, f: PairFunction, df_dx: PairFunction, df_dy: PairFunction): Fuzzy = o match {
    case Gaussian(mu2,sigma2) => Gaussian(f(mu,mu2),math.sqrt(f(sigma*sigma,sigma2*sigma2)))
    case _ => throw new UnsupportedOperationException("NYI")
  }
  override def toString = s"Gaussian: $mu, $sigma"
  def newFuzzy(x: Double, delta: Double) = Gaussian(x,delta)
  override def map2(f: DiFunc[Double])(delta2: Double) = throw new UnsupportedOperationException("NYI")
}
case class Bounded(mu: Double, delta: Double) extends FuzzyBase(mu,delta,new UniformRealDistribution(mu-math.abs(delta),mu+math.abs(delta))) {
  require(delta>0,"delta must be positive")
  def combine(o: Fuzzy, f: PairFunction, df_dx: PairFunction, df_dy: PairFunction): Fuzzy = o match {
    case Bounded(mu2,delta2) =>
      val t = mu -> mu2
      Bounded(f.tupled(t),math.abs(df_dx.tupled(t)*delta)+math.abs(df_dy.tupled(t)*delta2))
    case _ => throw new UnsupportedOperationException("NYI")
  }
  override def toString = s"Bounded: $mu, $delta"
  def newFuzzy(x: Double, delta: Double) = Bounded(x,delta)
}
case class General(dist: AbstractRealDistribution) extends FuzzyBase(dist.getNumericalMean,math.sqrt(dist.getNumericalVariance),dist) {
  def combine(o: Fuzzy, f: PairFunction, df_dx: PairFunction, df_dy: PairFunction): Fuzzy = o match {
    case _ => throw new UnsupportedOperationException("NYI")
  }
  override def toString = s"General: $dist"
  def newFuzzy(x: Double, delta: Double) = Gaussian(x,delta)
  override def map2(f: DiFunc[Double])(delta2: Double) = throw new UnsupportedOperationException("NYI")
}

object Fuzzy {
  implicit object FuzzyNumeric extends FuzzyIsFractional
  implicit def intToFuzzy(x: Int) = Exact(x)
  val parser = new FuzzyParser
  def apply(i: String, x: Option[String], f: Option[String], e: Option[String]): Fuzzy = {
    def exponent(n: Int) = math.pow(10,n)
    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =  for { aa <- a;	bb <- b	} yield f(aa,bb)
    def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A,B,C) => D): Option[D] = for { aa <- a; bb <- b; cc <- c } yield f(aa,bb,cc)
    def p(x: (Double,Int)) = (i.toInt+x._1)
    def createFuzzyXEF(x: (Double,Int), e: Double, f: Double): Fuzzy = Gaussian(p(x)*e,f)
    def createFuzzyXE(x: (Double,Int), e: Double): Fuzzy = Exact(p(x)*e)
    def createFuzzyXF(x: (Double,Int), f: Double): Fuzzy = Gaussian(p(x),f)
    def createFuzzyX(x: (Double,Int)): Fuzzy = Exact(p(x))
    val exp = for (z <- e) yield exponent(z.toInt)
    val fraction = for (z <- x) yield (z.toDouble,z.length)
    def fuzzFactor(z: String, n: Int) = z.toInt*exponent(z.length-n)
    val fuzz = for { z <- f; w <- fraction; y <- (exp orElse Some(1.0)) } yield fuzzFactor(z,w._2)*y
//    println(s"creating Fuzzy: with parameters: $i $x $f $e with intermediate results fraction=$fraction, fuzz=$fuzz, exponent=$exponent")
    // FIXME this is not really going to work: it creates a Gaussian without exponent in preference to an Exact with exponent. 
    map3(fraction,exp,fuzz)(createFuzzyXEF) orElse map2(fraction,fuzz)(createFuzzyXF) orElse map2(fraction,exp)(createFuzzyXE) orElse (fraction map {createFuzzyX _}) match {
      case Some(q) => q
      case None => Exact(i.toInt)
    }
  }
  def parse(s: String): Try[Fuzzy] = 
    parser.parseAll(parser.fuzzy,s) match {
      case parser.Success(x,_) => Success(x)
      case parser.Failure(_,z) => Failure(new RuntimeException(s"parser failure: $z"))
      case parser.Error(_,f) => Failure(new RuntimeException(s"parser error: $f"))
    }
  val zero = Exact(0)
  val one = Exact(1)
  val two = Exact(2)
  def product(x: Fuzzy, y: Fuzzy): Fuzzy = x match {
    case Exact(p) => y match { case Exact(q) => Exact(p*q); case _ => product(y, x) }
    case _ => x.map2(Times(y.get))(y.fuzz)
  }
  def sum(x: Fuzzy, y: Fuzzy): Fuzzy = x match {
    case Exact(p) => y match { case Exact(q) => Exact(p+q); case _ => sum(y,x) }
    // FIXME try to eliminate the following cast
    case _ => x.asInstanceOf[FuzzyBase].combine(y, _+_, (x,y)=>1, (x,y)=>1)
  }
  def invert(x: Fuzzy) = x.map(Inverse)

  trait FuzzyIsFractional extends Fractional[Fuzzy] {
    def plus(x: Fuzzy, y: Fuzzy): Fuzzy = sum(x,y)
    def minus(x: Fuzzy, y: Fuzzy): Fuzzy = sum(x,negate(y))
    def times(x: Fuzzy, y: Fuzzy): Fuzzy = product(x,y)
    def quot(x: Fuzzy, y: Fuzzy): Fuzzy = product(x,invert(y))
    def negate(z: Fuzzy): Fuzzy = product(z,Exact(-1))
    def fromInt(x: Int): Fuzzy = Exact(x)
    def rem(x: Fuzzy, y: Fuzzy): Fuzzy = zero
    def toInt(g: Fuzzy): Int = toLong(g).toInt
    def toLong(g: Fuzzy): Long = g match {
      case Exact(x) => x.toLong
      case _ => throw new UnsupportedOperationException(s"toLong: $g (not exact)") 
    }
    def toFloat(g: Fuzzy): Float = toDouble(g).toFloat
    def toDouble(g: Fuzzy): Double = g match {
      case Exact(x) => x.toDouble
      case _ => throw new UnsupportedOperationException(s"toDouble: $g (not exact)") 
    }
    // Members declared in scala.math.Fractional
    def div(x: Fuzzy,y: Fuzzy): Fuzzy = product(x,invert(y))
    
    // Members declared in scala.math.Ordering
    def compare(x: Fuzzy,y: Fuzzy): Int = compare(x,y)
  }  
}
