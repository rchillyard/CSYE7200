package edu.neu.coe.csye7200

package object benchmark {

  implicit class Rep(n: Int) {
    /**
      * Method which can be invoked, provided that Benchmark._ has been imported.
      * See for example BenchmarkSpec
      *
      * @param f the function to be invoked
      * @tparam A the result type of f
      * @return the average number of nano-seconds per run
      */
    def times[A](f: => A): Double = {
      // Warmup phase: do at least 20% of repetitions before starting the clock
      1 to (1 + n / 5) foreach (_ => f)
      val start = System.nanoTime()
      1 to n foreach (_ => f)
      (System.nanoTime() - start) / n.toDouble
    }
  }

}
