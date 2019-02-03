package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 1/15/17.
  */
trait Foo {
  def a: String

  def create(a: String): Foo
}

case class Bar(a: String, b: Option[Int]) extends Foo {
  def create(a: String) = Bar(a, None)
}

//noinspection NameBooleanParameters
case class Buzz(a: String, b: Boolean) extends Foo {
  def create(a: String) = Buzz(a, false)
}
