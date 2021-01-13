package edu.neu.coe.csye7200.assthw

/**
  * This is the second part of assignment "helloworld."
  * You should be able to run this program with provided csv file using sbt run.
  * You task is to replace line 38 with the following line of code:
  * println((for (m <- ingester(source); if (m.properties(20)=="New Zealand")) yield m).size)
  * Run this program with provided csv file, and submit a screenshot of the result.
  * It should print the number of Kiwi (New Zealand) Movies.
  */

import scala.io.Source

trait Ingestible[X] {
  def fromStrings(ws: Seq[String]): X
}

class Ingest[T: Ingestible] extends (Source => Iterator[T]) {
  def apply(source: Source): Iterator[T] = source.getLines().toSeq.map(e => implicitly[Ingestible[T]].fromStrings(e.split(",").toList)).iterator
}

case class Movie(properties: Seq[String])

object Ingest extends App {

  trait IngestibleMovie extends Ingestible[Movie] {
    def fromStrings(ws: Seq[String]): Movie = Movie.apply(ws)
  }

  implicit object IngestibleMovie extends IngestibleMovie

  val ingester = new Ingest[Movie]()
  val source = args.toList match {
    case Nil => Source.fromResource("movie_metadata_5000.csv")
    case h :: _ => Source.fromFile(h)
  }

  for (m <- ingester(source)) println(m.properties.mkString(", "))
  source.close()

  // Please note that an alternative to the definition of source above would be as in the following comment:
  //  val source = Source.fromFile(if (args.length>0) args.head else "movie_metadata_5000.csv")
}
