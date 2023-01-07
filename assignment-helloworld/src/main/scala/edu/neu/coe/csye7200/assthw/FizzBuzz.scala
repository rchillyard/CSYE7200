package edu.neu.coe.csye7200.assthw

/**
 * An implementation of FizzBuzz that uses pattern-matching rather than (redundant) if clauses.
 *
 * This method uses a rather advance pattern-matching trick: declaring our own unapply method for Factor.
 */
object FizzBuzz extends App {
    def fizzBuzz(x: Int): String = {
        lazy val dividesBy3 = Factor(3)
        lazy val dividesBy5 = Factor(5)
        lazy val dividesBy3And5 = Factor(3 * 5)
        x match {
            case dividesBy3And5(_) => "FizzBuzz"
            case dividesBy3(_) => "Fizz"
            case dividesBy5(_) => "Buzz"
            case _ => x.toString
        }
    }

    private val strings = for (x <- 1 to 100) yield fizzBuzz(x)
    println(strings mkString("", "\n", ""))
}

/**
 * Case class to model the concept of factors.
 *
 * @param f a factor.
 */
case class Factor(f: Int) {
    /**
     * Determine whether f is a factor of x.
     *
     * @param x a potential multiple of f.
     * @return true if f is a factor of x.
     */
    def isMultiple(x: Int): Boolean = x % f == 0

    /**
     * Unapply method for this Factor.
     *
     * @param x a candidate multiple of f.
     * @return Some(quotient) if x is a multiple of f; otherwise, None.
     */
    def unapply(x: Int): Option[Int] = if (isMultiple(x)) Some(x / f) else None
}
