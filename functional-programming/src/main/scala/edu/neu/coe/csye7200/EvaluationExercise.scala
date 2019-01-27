package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 2/17/17.
  */
object EvaluationExercise extends App {

  var count: Int = 0
  val a = {
    count += 1; count
  }
  lazy val b = {
    count += 1; count
  }

  def c = {
    count += 1; count
  }

  def d(x: => Int) = {
    count
  }

  (1 to 3).foreach { _ => a }
  println(count)
  (1 to 3).foreach { _ => b }
  println(count)
  (1 to 3).foreach { _ => c }
  println(count)
  (1 to 3).foreach { _ => d(a) }
  println(count)
}
