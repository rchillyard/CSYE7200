package edu.neu.coe.csye7200.fp.crypto

/**
  * Created by scalaprof on 3/10/17.
  */
class Factorization {

  def primeFactors(N: Long): Seq[Long] = {
    // TODO flesh this out
    def inner(r: Seq[Long], n: Long): Seq[Long] = r

    inner(Nil, N)
  }
}
