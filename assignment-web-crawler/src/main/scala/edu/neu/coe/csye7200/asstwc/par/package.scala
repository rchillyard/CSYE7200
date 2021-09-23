package edu.neu.coe.csye7200.asstwc

package object par {

  implicit class Rep(n: Int) {
    /**
     * Method which can be invoked, provided that Benchmark._ has been imported.
     * See for example BenchmarkSpec
     *
     * @param f the function to be invoked.
     * @tparam A the result type of f.
     * @return a tuple of the function result and the average number of milli-seconds per run.
     */
    def times[A](f: => A): (A, Double) = {
      // Warmup phase: do at least 20% of repetitions before starting the clock
      1 to (1 + n / 5) foreach (_ => f)
      val start = System.nanoTime()
      1 to n foreach (_ => f)
      val r = (System.nanoTime() - start) / n.toDouble / 1E6
      (f, r)
    }
  }

}
