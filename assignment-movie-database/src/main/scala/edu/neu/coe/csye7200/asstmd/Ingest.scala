package edu.neu.coe.csye7200.asstmd

import scala.io.Source
import scala.util.Try

class Ingest[T: Ingestible] extends (Source => Iterator[Try[T]]) {
  def apply(source: Source): Iterator[Try[T]] = source.getLines.toSeq.drop(1).map(e => implicitly[Ingestible[T]].fromString(e)).iterator
}

trait Ingestible[X] {
  def fromString(w: String): Try[X]
}
