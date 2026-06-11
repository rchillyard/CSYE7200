package edu.neu.coe.csye7200.labsorted

package object benchmark {

  /**
   * Implicit extension method to measure the average execution time of a computation over multiple iterations.
   *
   */
  extension (n: Int)
    /**
     * Measures the average execution time of a provided computation over a number of iterations.
     * The method automatically includes a warmup phase to ensure better performance measurement accuracy.
     *
     * @param f a computation or function to be executed repeatedly
     * @tparam A the result type of the computation
     * @return the average execution time in nanoseconds per execution
     */
    def times[A](f: => A): Double = {
      // Warmup phase: do at least 20% of repetitions before starting the clock
      1 to (1 + n / 5) foreach (_ => f)
      val start = System.nanoTime()
      1 to n foreach (_ => f)
      (System.nanoTime() - start) / n.toDouble
    }
}


