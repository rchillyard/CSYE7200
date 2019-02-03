/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

import com.phasmid.laScala.values.Scalar

/**
  * Created by scalaprof on 1/13/17.
  */
package object parse {

  type Expression = Either[Scalar, Invocation]

  type Lookup[T] = () => String => Option[T]
}
