import LazyList._

val b: Double = LazyList.from(1).map(n => 1.0 / (n * n)).take(10000).foldLeft(0.0)(_ + _)
println(s"Sum: $b")