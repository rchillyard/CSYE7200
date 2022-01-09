package edu.neu.coe.csye7200.numerics

import edu.neu.coe.csye7200.numerics.EulerBrick.{hasCorrectFactors, isEulerTriple}

import scala.language.postfixOps

case class EulerBrick(a: Long, b: Long, c: Long) {

  def isValid: Boolean = hasCorrectFactors(a, b, c) && isEulerTriple(a, b, c)

}

object EulerBrick {

  implicit class Factored(x: Long) {
    def hasFactor(y: Long): Boolean = x % y == 0L
  }

  def apply(t: (Long, Long, Long)): EulerBrick = EulerBrick(t._1, t._2, t._3)

  def makeList(x: Int): LazyList[EulerBrick] = makeList(LazyList.from(x).map(_.toLong).flatMap(makeEulerTriples))

  // NOTE the filter for valid EulerBricks is not strictly necessary.
  def makeList(ts: LazyList[(Long, Long, Long)]): LazyList[EulerBrick] = ts map apply filter (_.isValid)

  def makeBricks(x: Int): List[EulerBrick] = makeList(1) take x to List

  def stepFactorOr(y: Long, factored: Boolean, factor: Long): Long = if (factored || (y hasFactor factor)) 1L else factor

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

  def getSuitableX(y: Long, step: Long): Seq[Long] = step until (y, step)

  def hasCorrectFactors(xs: Long*): Boolean = xs.exists(_ hasFactor 11) && xs.count(_ hasFactor 4) > 1 && xs.count(_ hasFactor 3) > 1

  def isEulerTriple(t: (Long, Long, Long)): Boolean =
    Pythagorean.isValid(t._1, t._2) && Pythagorean.isValid(t._2, t._3) && Pythagorean.isValid(t._3, t._1)

  def makeEulerTriples(x: Long): LazyList[(Long, Long, Long)] = makeTriples(x) filter isEulerTriple
}
