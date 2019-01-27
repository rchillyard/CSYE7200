package edu.neu.coe.csye7200

/**
  * Created by scalaprof on 12/1/16.
  */
case class InvariantType[+A](x: A) {

  def compare[B >: A](y: B): Boolean = x == y

}
