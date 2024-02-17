package edu.neu.coe.csye7200.prime

object PrimeFun extends App {

  // NOTE: This exercise concerns the values of p^2 % n where p is a prime number and n is a "magic number."
  // You can get a lazy list of primes from edu.neu.coe.csye7200.prime.Prime.primes

  // TODO read a set of numbers from the command line (set these with menu item: Run/Edit Configuration ... Program arguments).
  // For each number (called the magicNumber) write out the number and the first 100 values, skipping the first two.
  // The numbers should start after 12 and you shouldn't need more than 12 to see the pattern.
  // The pattern should be obvious in just the first 10 results.
  // Submit the file (Question 1)

  // TODO using the one magic number that gives you the pattern, get a list of the first 100,000 numbers (again excluding the first two).
  // Try to find the first number that doesn't match the pattern (there may be none).

  private val numbers: Array[Int] = args map (_.toInt)

  private def show(magicNumber: Int): String = {
    // TO BE IMPLEMENTED 
     ???
    // END

    s"""$magicNumber: ${ys.mkString(",")}"""
  }

  for (n <- numbers) println(show(n))
}