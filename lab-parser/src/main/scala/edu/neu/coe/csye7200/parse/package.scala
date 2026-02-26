/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200

import com.phasmid.laScala.values.Scalar
import edu.neu.coe.csye7200.shuntingyard.{Operator, Parenthesis}

/**
  * Created by scalaprof on 1/13/17.
  */
package object parse {

  /**
   * A type alias defining `Expression` as an `Either` type, where:
   * - `Left` represents a `Scalar`,
   * - `Right` represents an `Invocation`.
   *
   * This type captures either a singular scalar value or a functional invocation within an expression parsing context.
   */
  type Expression = Either[Scalar, Invocation]

  /**
   * Type alias representing a function that accepts no arguments and returns another function.
   * The returned function takes a `String` as an argument and produces an `Option[T]`.
   *
   * The `Lookup` type can be used in scenarios where a context-dependent or deferred lookup mechanism
   * is required. For example, it might represent a closure that fetches a value or resolves a key
   * based on a runtime `String` input within some encapsulating context.
   *
   * @tparam T the type of the result encapsulated in an `Option`, representing the lookup result.
   */
  type Lookup[T] = () => String => Option[T]

  /**
   * A type alias representing a token used in the Shunting Yard algorithm.
   * A token can be one of three possible types:
   * - `Parenthesis` represented by `Left(Parenthesis)`
   * - `Operator` represented by `Right(Left(Operator))`
   * - `Int` values represented by `Right(Right(Int))`
   */
  type Token = Either[Parenthesis, Either[Operator, Int]]

}