package edu.neu.coe.csye7200.assthw

import scala.util.Random

object Fall2024Quiz1 extends App {
  val random = new Random(0L)

  // TODO (A: 7) build a lazy list (indexedRandom) of random numbers each in the range 0-1 together with an index which starts at 0
  // hint: you will need to invoke LazyList.continually.
  // If you use zipWithIndex, you will need to swap the positions of each tuple.
  // An example is shown (firstIndexedElement) which you do NOT need to use!

  val firstIndexedElement: (Int, Double) = (0, random.nextDouble())

  val indexedRandom: LazyList[(Int, Double)] = LazyList.from(1).zip(LazyList.continually(random.nextDouble()))

  println(indexedRandom take 10 to List)

  // TODO (B: 4) now take a finite subset of indexedRandom which consists of the first 10 elements.

  val firstTen: List[(Int, Double)] = ???

  // TODO (C: 3) print the list with commas as the separator

  println(???)

  // TODO (D: 8) starting with indexedRandom again, generate a list of the first 1000 elements,
  //  skipping the first ten that you already used (hint: use slice)
  // and this time throw away the index (hint: use unzip)

//  val (xs: List[Int], thousandRandom: List[Double]) = (???, ???)

  // TODO (E: 4) create a list from the thousandRandom where the elements are the square roots (math.sqrt) of the original.
  // Bonus points (2) if there are no parentheses or underscores in the expression you write.

  val squareRoots: List[Double] = ???

  // TODO (F: 5) evaluate the mean of the square roots.

  val meanSquareRoot: Double = ???

  // TODO (G: 2) print the mean square root as: "The mean square root is: x" where x is the value of the mean square root.
  println(???)

  // TODO (H: 7) generate a lazy list of random numbers from random in pairs.
  val pairs: LazyList[(Double, Double)] = ???

  // TODO (I: 5) generate a lazy list where each element is the larger of the corresponding pair.
  val larger: LazyList[Double] = ???

  // TODO (J: 3) filter "larger" by retaining only the elements smaller than 0.5
  val smallValues: LazyList[Double] = ???

  // TODO (K: 9) introduce a method called mean which will take a list and return the list
  //  (previously, you simply evaluated the mean for a specific list).
  // Invoke your method on a list derived from larger.
  // You must use the signature provided below (with a parametric type).
  // NOTE: you will need to invoke implicitly[Numeric[X]] in order to get the sum as a Double.

  def mean[X: Numeric](xs: List[X]): Double = ???

  // TODO (L: 6) print the mean value for the first 1000 values of larger.
  // NOTE: larger is a lazy list, not a list.
  // Compare this value with the other mean and discuss (just a sentence or two).
  // NOTE You will probably want to run the program again several times but without specifying a seed for random.

  println(???)

  // NOTE: See video at https://www.youtube.com/shorts/UJfTBQRY2Cs

}