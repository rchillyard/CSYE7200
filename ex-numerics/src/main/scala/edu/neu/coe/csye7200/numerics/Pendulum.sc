/**
 * An experiment is sent to a newly discovered moon of one of the major solar system planets to determine the acceleration due to gravity (g) on the surface of this moon.
 * You are presented with the experimental data in two lists: pendulum lengths (in meters) and oscillation times (in seconds). Your job is to determine the mean value of the g.
 * The relevant formula that you need is:
 * g = l (2π/T)**2
 * where l is the length and T is the time (see the dataset below).
 * You must use the map2 method that you developed in the previous question to yield the list of g-values.
 * Finally, calculate the mean of the g-values using reduce or foldLeft.
 */

def map2(l: Seq[Double], t: Seq[Double])(f: (Double, Double) => Double): Seq[Double] = for (l1 <- l; t1 <- t) yield f(l1, t1)

def sqr(x: Double): Double = x * x

def gDef(l: Double, t: Double): Double = l * sqr((2 * math.Pi) / t)

val ls = List(0.5, 0.75, 1, 1.25, 1.5, 2, 5)
val ts = List(3.821, 4.800, 5.357, 6.222, 6.688, 7.644, 12.264)

val zs: Seq[(Seq[Double], Seq[Double])] = (ls zip ts) map (x => Seq(x._1) -> Seq(x._2))
val gs: Seq[Seq[Double]] = for (z <- zs) yield map2(z._1, z._2)(gDef)
val mean: Double = gs.foldLeft(0.0) { case (a, Seq(x)) => a + x } / gs.size

println(s"Mean: $mean")