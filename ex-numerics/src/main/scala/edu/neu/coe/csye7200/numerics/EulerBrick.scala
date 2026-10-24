package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.EulerBrick.{hasCorrectFactors, isEulerTriple}
import scala.language.postfixOps

/**
 * Represents an Euler Brick, a right rectangular cuboid whose edges and face diagonals are integers.
 *
 * @constructor Creates an EulerBrick instance with the given edge lengths.
 * @param a The length of one edge of the cuboid.
 * @param b The length of another edge of the cuboid.
 * @param c The length of the third edge of the cuboid.
 */
case class EulerBrick(a: Long, b: Long, c: Long) {

  /**
   * Determines the validity of the Euler Brick based on its edge lengths.
   *
   * The method checks if the edge lengths satisfy the conditions of having the
   * correct factors and forming an Euler triple (a set of three integers such that
   * all the square sums of two sides equal the square of a diagonal).
   *
   * @return `true` if the Euler Brick is valid, `false` otherwise
   */
  def isValid: Boolean = hasCorrectFactors(a, b, c) && isEulerTriple(a, b, c)
}

/**
 * Companion object for the EulerBrick class. Provides utility methods to construct instances of EulerBrick
 * and generate sequences of Euler Bricks or their candidate triples.
 */
object EulerBrick {

  /**
   * Extension method for `Long` to check if the given number has a specific factor.
   *
   */
  extension (x: Long) {
    /**
     * Determines whether the given number `y` is a factor of the number to which this method is applied.
     *
     * @param y The number to check as a potential factor.
     * @return `true` if `y` is a factor of the number, `false` otherwise.
     */
    def hasFactor(y: Long): Boolean = x % y == 0L
  }

  /**
   * Creates an instance of `EulerBrick` from a tuple of three edge lengths.
   *
   * @param t A tuple containing three `Long` values representing the edge lengths of the Euler Brick.
   *          The tuple elements correspond to the edge lengths `a`, `b`, and `c` respectively.
   *
   * @return An `EulerBrick` instance initialized with the specified edge lengths.
   */
  def apply(t: (Long, Long, Long)): EulerBrick = EulerBrick(t._1, t._2, t._3)

  /**
   * Creates a lazy list of EulerBrick instances starting from the given integer.
   *
   * This method takes an integer value `x`, generates a lazy list of values 
   * starting from `x` converted to Long, and applies a transformation to produce 
   * candidate (Long, Long, Long) triples corresponding to Euler Bricks. It then constructs
   * EulerBrick instances from those triples.
   *
   * @param x the starting integer value for generating the lazy list of candidate Euler Bricks
   * @return a lazy list of EulerBrick instances
   */
  def makeList(x: Int): LazyList[EulerBrick] = makeList(LazyList.from(x).map(_.toLong).flatMap(makeEulerTriples))

  /**
   * Generates a lazy list of valid Euler Bricks from a given lazy list of triples.
   *
   * Each triple is converted into an EulerBrick object, and only valid instances
   * (i.e., those that satisfy the `isValid` condition) are included in the resulting lazy list.
   *
   * @param ts a lazy list of tuples, where each tuple contains three `Long` values 
   *           representing potential edge lengths of an Euler Brick.
   *
   * @return a lazy list of `EulerBrick` instances that are valid according to the `isValid` method.
   */
  // NOTE the filter for valid EulerBricks is not strictly necessary.
  def makeList(ts: LazyList[(Long, Long, Long)]): LazyList[EulerBrick] = ts map apply filter (_.isValid)

  /**
   * Creates a list of Euler Bricks up to a specified quantity.
   *
   * This method generates a finite list of valid Euler Bricks
   * by taking a predefined lazy list and limiting it to the specified quantity.
   *
   * @param x The number of Euler Bricks to generate. Must be a non-negative integer.
   * @return A list containing up to `x` Euler Bricks.
   */
  def makeBricks(x: Int): List[EulerBrick] = makeList(1) take x to List

  /**
   * Computes a step value based on whether a factor is applied and whether a given number `y` has the specified factor.
   *
   * @param y        the number to be checked for the factor.
   * @param factored a boolean indicating whether the factor is already applied.
   * @param factor   the factor to be evaluated against `y`.
   * @return the step value, which is 1 if `factored` is true or if `y` has the `factor`; otherwise, it returns the value of `factor`.
   */
  def stepFactorOr(y: Long, factored: Boolean, factor: Long): Long = if (factored || (y hasFactor factor)) 1L else factor

  /**
   * Determines the step factor based on whether a number `y` is factored and has a specific factor.
   * If `factored` is true and `y` has the given `factor`, the step factor returned is 1L.
   * Otherwise, the method returns the given factor.
   *
   * @param y        the number to check for the factor
   * @param factored boolean indicating whether the number is factored
   * @param factor   the factor to evaluate
   * @return 1L if `factored` is true and `y` has `factor`, otherwise the provided `factor`
   */
  def stepFactorAnd(y: Long, factored: Boolean, factor: Long): Long = if (factored && (y hasFactor factor)) 1L else factor

  /**
   * Make a lazy list of (Long, Long, Long), that's to say, triples which are candidate solutions for the edges
   * of an Euler Brick.
   * NOTE not all such triples will be valid.
   *
   * @param z the value of the longest edge of a potential Euler Brick.
   * @return a tuple of three Longs representing x, y, and z.
   */
  def makeTriples(z: Long): LazyList[(Long, Long, Long)] = {
    val zHas11 = z hasFactor 11
    val zHas4 = z hasFactor 4
    val zHas3 = z hasFactor 3
    val ts = for (y <- 1L until z;
                  step = 1L * stepFactorOr(y, zHas11, 11) * stepFactorAnd(y, zHas4, 4) * stepFactorAnd(y, zHas3, 3);
                  x <- getSuitableX(y, step)
                  ) yield (x, y, z)
    ts.to(LazyList)
  }

  /**
   * Generates a sequence of numbers between a specified range with a given step.
   *
   * This method produces a sequence of numbers starting from `step` and incrementing by `step`
   * until it reaches or surpasses the value `y`.
   *
   * @param y    the upper limit of the range (exclusive); numbers in the sequence will be less than `y`.
   * @param step the step size to increment by for each value in the sequence.
   * @return a sequence of `Long` values that are evenly spaced by `step` and less than `y`.
   */
  def getSuitableX(y: Long, step: Long): Seq[Long] = step until(y, step)

  /**
   * Determines whether the given sequence of numbers meets specific factorization criteria.
   * The method checks three conditions:
   * 1. At least one number in the sequence must have a factor of 11.
   * 2. There must be more than one number in the sequence that has a factor of 4.
   * 3. There must be more than one number in the sequence that has a factor of 3.
   *
   * @param xs A varargs sequence of `Long` numbers to evaluate against the factorization criteria.
   * @return `true` if all the factorization conditions are satisfied; `false` otherwise.
   */
  def hasCorrectFactors(xs: Long*): Boolean = xs.exists(_ hasFactor 11) && xs.count(_ hasFactor 4) > 1 && xs.count(_ hasFactor 3) > 1

  /**
   * Determines whether the given tuple of three `Long` values forms an Euler triple.
   * An Euler triple satisfies the property that each pair within the tuple forms 
   * a valid Pythagorean pair as determined by the `Pythagorean.isValid` method.
   *
   * @param t A tuple containing three `Long` values, representing potential edge lengths of an Euler Brick.
   *          The tuple is interpreted as (a, b, c), where a, b, and c are the edge lengths.
   *
   * @return `true` if the tuple forms an Euler triple, `false` otherwise.
   */
  def isEulerTriple(t: (Long, Long, Long)): Boolean =
    Pythagorean.isValid(t._1, t._2) && Pythagorean.isValid(t._2, t._3) && Pythagorean.isValid(t._3, t._1)

  /**
   * Filters and generates a lazy list of Euler triples from candidate triples.
   *
   * This method takes a starting number `x`, generates candidate triples using the `makeTriples` method,
   * and filters the results to include only those triples that satisfy the `isEulerTriple` condition.
   *
   * @param x the starting value from which candidate triples will be generated.
   * @return a lazy list of triples (Long, Long, Long) that are valid Euler triples.
   */
  def makeEulerTriples(x: Long): LazyList[(Long, Long, Long)] = makeTriples(x) filter isEulerTriple
}