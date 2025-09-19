package edu.neu.coe.csye7200.prime

/**
 * Entry point of the program that performs calculations based on prime numbers and given "magic numbers" from
 * the command-line arguments. It computes the values of p^2 % n for primes p and a provided n, where n is a "magic number."
 * This demonstrates patterns in modular arithmetic using prime squares.
 *
 * Unfortunately, I can't quite remember how this is supposed to work (or what the program args are supposed to be)
 *
 * @param args The command-line arguments, which are expected to be a sequence of "magic numbers" as strings.
 * @return No direct return value. The results are printed to standard output.
 */
@main def PrimeFun(args: String*): Unit = {

  // NOTE: This exercise concerns the values of p^2 % n where p is a prime number and n is a "magic number."
  // You can get a lazy list of primes from edu.neu.coe.csye7200.prime.Prime.primes

  // TODO read a set of numbers from the command line (set these with menu item: Run/Edit Configuration ... Program arguments).
  // For each number (called the magicNumber) write out the number and the first 100 values, skipping the first two.
  // The numbers should start after 12 and you shouldn't need more than 12 to see the pattern.
  // The pattern should be obvious in just the first 10 results.
  // Submit the file (Question 1)

  // TODO using the one magic number that gives you the pattern, get a list of the first 100,000 numbers (again excluding the first two).
  // Try to find the first number that doesn't match the pattern (there may be none).

  val numbers: Seq[Int] = args map (_.toInt)

  def show(magicNumber: Int): String = {
    // TO BE IMPLEMENTED 
  ???
    // END

    s"""$magicNumber: ${ys.mkString(",")}"""
  }

  for (n <- numbers) println(show(n))
}