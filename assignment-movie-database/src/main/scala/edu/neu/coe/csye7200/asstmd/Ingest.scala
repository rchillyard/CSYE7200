package edu.neu.coe.csye7200.asstmd

import scala.io.Source
import scala.util.Try

/**
  * Class which will read a Source, line by line, and parse each line as a Try[T].
  * The first line is expected to be a header line and so is ignored.
  *
  * @tparam T the underlying type that we need to ingest.
  */
class Ingest[T: Parsable] extends (Source => Iterator[Try[T]]) {
  def apply(source: Source): Iterator[Try[T]] = source.getLines().drop(1).map(implicitly[Parsable[T]].parse)
}

/**
  * This trait defines a type class which implements a method "parse" that takes a String and returns a Try[X].
  *
  * @tparam X the parametric type of the Parsable interface.
  */
trait Parsable[X] {
  def parse(w: String): Try[X]
}
