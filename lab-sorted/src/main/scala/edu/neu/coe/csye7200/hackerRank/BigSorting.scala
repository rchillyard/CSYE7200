package edu.neu.coe.csye7200.hackerRank

import java.io._
import scala.annotation.tailrec

object BigSorting {

  // Complete the bigSorting function below.
  def bigSorting(unsorted: Array[String]): Array[String] = {
    val maxLength = unsorted.foldLeft(0)((largest, element) => if (element.length > largest) element.length else largest)

    // Starting with the least significant digits, sort the Strings according to digit i
    (0 to maxLength).foldLeft(unsorted)((array, i) => insertionSort(array, i))
  }

  /**
    * Run insertion sort on the array of Strings, but based only on the indexth digit
    *
    * @param array the array of Strings.
    * @param index the index of the digit to be compared (starting from the least significant end)
    * @return the result.
    */
  def insertionSort(array: Array[String], index: Int): Array[String] = {
    @tailrec
    def inner(i: Int, j: Int): Unit = {
      j match {
        case -1 =>
        case _ => if (less(array(i), array(j), index)) {
          swap(array, i, j)
          inner(i - 1, j - 1)
        }
      }
    }

    for (i <- array.indices) {
      inner(i, i - 1)
    }
    array
  }

  /**
    * Get the ith character, counting from the least significant digit, where i runs from 0 thru x.length-1
    *
    * @param x the String
    * @param i the index
    * @return the ith character
    */
  def digit(x: String, i: Int): Option[Char] = if (i >= x.length) None else x.drop(x.length - i - 1).headOption

  /**
    * Swap elements i and j of Array x
    *
    * @param x the Array
    * @param i first index
    * @param j second index
    */
  def swap[X](x: Array[X], i: Int, j: Int): Unit = if (i != j) {
    val temp = x(i)
    x(i) = x(j)
    x(j) = temp
  }

  /**
    * Compare x and y in their ith digit only and return true if x is less than y
    *
    * @param x first String
    * @param y second String
    * @param i index counting from length-1 thru 0
    * @return true if
    */
  def less(x: String, y: String, i: Int): Boolean = {
    digit(y, i) match {
      case None => false
      case Some(b) => digit(x, i) match {
        case None => true
        case Some(a) => a < b
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(System.out)

    val n = stdin.readLine().trim.toInt

    val unsorted = Array.ofDim[String](n)

    for (i <- 0 until n) {
      val unsortedItem = stdin.readLine()
      unsorted(i) = unsortedItem
    }

    val result = bigSorting(unsorted)

    printWriter.println(result.mkString("\n"))

    printWriter.close()
  }
}
