package edu.neu.coe.csye7200.labsorted.lbsort

import scala.util.Random

/**
 * Benchmark for the base-case cutoff in [[MergeSortList]].
 *
 * Two questions:
 *
 *  1. does deferring to a quadratic sort below some size actually pay, and where
 *     does it stop paying;
 *  2. which quadratic sort should it be -- [[InsertionSortList]] or
 *     [[BubbleSortList]].
 *
 * The headline measurement is the comparison count, not the clock. Counts are
 * deterministic: the same input gives the same number every run, with no warmup,
 * no JIT and no garbage collector in the way. Times are reported alongside as a
 * sanity check, taken as the best of several runs, but where the two disagree by
 * a small margin the counts are the ones to believe.
 *
 * The distribution matters more than anything else here. Uniform random input is
 * bubble sort's worst case: its early exit only fires on a pass that swaps
 * nothing, and merge sort's sublists at the bottom of the recursion are in random
 * order unless the original input had runs in it. So the nearly-sorted and sorted
 * rows are where bubble sort has a case to make.
 */
object SortCutoffBenchmark {

  /**
   * An Ordering which counts the comparisons made through it.
   *
   * This is how the sorts are instrumented without their knowing: they take an
   * Ordering, so passing one of these counts their comparisons from the outside.
   */
  class CountingOrdering[X](xo: Ordering[X]) extends Ordering[X] {
    private var counter: Long = 0L

    def compare(x: X, y: X): Int = {
      counter += 1
      xo.compare(x, y)
    }

    def count: Long = counter

    def reset(): Unit = counter = 0L
  }

  /**
   * A MergeSortList whose base case is BubbleSortList rather than
   * InsertionSortList.
   */
  class MergeSortListViaBubble[X: Ordering](cutoff: Int) extends MergeSortList[X](cutoff) {
    private val bubble = new BubbleSortList[X]

    override protected def sortShort(xs: List[X]): List[X] = bubble.sort(xs)
  }

  val cutoffs: Seq[Int] = Seq(1, 4, 8, 16, 24, 32, 48, 64)

  /**
   * The input shapes worth distinguishing. A cutoff sort's value depends far more
   * on these than on n.
   *
   * @param n      the length of list to generate.
   * @param random the source of randomness.
   * @return pairs of name and generator.
   */
  def distributions(n: Int, random: Random): Seq[(String, List[Int])] = Seq(
    "random" -> List.fill(n)(random.nextInt()),
    "sorted" -> (1 to n).toList,
    "reversed" -> (n to 1 by -1).toList,
    "nearly sorted" -> nearlySorted(n, n / 50, random),
    "few unique" -> List.fill(n)(random.nextInt(8))
  )

  /**
   * A sorted list disturbed by a given number of random transpositions.
   */
  def nearlySorted(n: Int, swaps: Int, random: Random): List[Int] = {
    val a = (1 to n).toArray
    for (_ <- 0 until swaps) {
      val i = random.nextInt(n)
      val j = random.nextInt(n)
      val t = a(i)
      a(i) = a(j)
      a(j) = t
    }
    a.toList
  }

  /**
   * Count the comparisons one sort makes on one input.
   */
  def comparisons(xs: List[Int], sorter: Ordering[Int] => List[Int] => List[Int]): Long = {
    val counting = new CountingOrdering[Int](Ordering[Int])
    val result = sorter(counting)(xs)
    // Touch the result so that it cannot be optimised away.
    require(result.lengthCompare(xs.length) == 0)
    counting.count
  }

  /**
   * Time one sort on one input, taking the best of `reps` runs.
   */
  def milliseconds(xs: List[Int], sort: List[Int] => List[Int], reps: Int): Double = {
    var best = Double.MaxValue
    var sink = 0
    for (_ <- 0 until reps) {
      val t0 = System.nanoTime()
      val result = sort(xs)
      val elapsed = (System.nanoTime() - t0) / 1e6
      sink += result.length
      if (elapsed < best) best = elapsed
    }
    require(sink >= 0)
    best
  }
}

/**
 * Run the cutoff benchmark. Prints a table per input distribution.
 */
@main def doBenchmarkSortCutoff(): Unit = {

  import SortCutoffBenchmark.*

  val n = 100000
  val reps = 10
  val random = new Random(42)

  def insertionBased(cutoff: Int)(using Ordering[Int]): List[Int] => List[Int] =
    MergeSortList.withCutoff[Int](cutoff).sort

  def bubbleBased(cutoff: Int)(using Ordering[Int]): List[Int] => List[Int] =
    new MergeSortListViaBubble[Int](cutoff).sort

  println(s"MergeSortList base-case cutoff, n = $n, best of $reps runs")
  println("cutoff 1 disables the base case, i.e. plain merge sort\n")

  for ((name, xs) <- distributions(n, random)) {
    // Warm up on this input before timing anything.
    for (_ <- 0 until 3) {
      val _ = insertionBased(16)(using Ordering[Int])(xs)
      val _ = bubbleBased(16)(using Ordering[Int])(xs)
    }

    println(f"  $name%-14s ${"cutoff"}%7s ${"insertion cmps"}%15s ${"ms"}%8s ${"bubble cmps"}%14s ${"ms"}%8s")
    for (cutoff <- cutoffs) {
      val ci = comparisons(xs, o => insertionBased(cutoff)(using o))
      val ti = milliseconds(xs, insertionBased(cutoff)(using Ordering[Int]), reps)
      val cb = comparisons(xs, o => bubbleBased(cutoff)(using o))
      val tb = milliseconds(xs, bubbleBased(cutoff)(using Ordering[Int]), reps)
      println(f"  ${""}%-14s $cutoff%7d $ci%15d $ti%8.1f $cb%14d $tb%8.1f")
    }
    println()
  }
}
