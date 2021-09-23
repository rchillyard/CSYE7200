package edu.neu.coe.csye7200.fp.minidatabase2

import scala.io.Source
import scala.util._

/**
  * @author scalaprof
  */
object MiniDatabase2 extends App {

  // Similar to the map2 you already know (4 points)
  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = ??? // TO BE IMPLEMENTED

  // Similar to the map2 you already know (4 points)
  def map2[A, B, C](a: Try[A], b: Try[B])(f: (A, B) => C): Try[C] = ??? // TO BE IMPLEMENTED

  def load(filename: String) = {
    val src = Source.fromFile(filename)
    val database = src.getLines().toList.map(e => Entry.parse(e.split(",")))
    val result = database
    src.close
    result
  }

  def measure(height: Height) = height match {
    case Height(8, _) => "giant"
    case Height(7, _) => "very tall"
    case Height(6, _) => "tall"
    case Height(5, _) => "normal"
    case Height(_, _) => "short"
  }


  if (args.length > 0) {
    val db = load(args(0))
    print(db)
  }
}

case class Entry(name: Name, height: Height)

case class Height(feet: Int, in: Int) {
  def inches: Int = feet * 12 + in
}

object Entry {
  def parse(name: Try[Name], height: Try[Height]): Try[Entry] = MiniDatabase2.map2(name, height)(Entry.apply)

  def parse(name: String, height: String): Try[Entry] = parse(Name.parse(name), Height.parse(height))

  def parse(entry: Seq[String]): Try[Entry] = Try {
    (entry.head, entry(3))
  }.flatMap { case (n, h) => parse(n, h) }
}

object Height {
  private val rHeightFtIn = """^\s*(\d+)\s*(?:ft|\')(\s*(\d+)\s*(?:in|\"))?\s*$""".r

  def parse(ft: String, in: String): Try[Height] = {
    val tryFt = Try(ft.toInt)
    val tryIn = Try(in.toInt)
    MiniDatabase2.map2(tryFt, tryIn)(Height.apply)
  }

  def parse(height: String): Try[Height] = height match {
    case rHeightFtIn(ft, _, in) => Height.parse(ft, in)
    case _ => Failure(new IllegalArgumentException(height))
  }

}

case class Name(first: String, middle: Option[String], last: String)

object Name {
  def sequence[T](o: Option[T], f: => Failure[T]): Try[T] = o match {
    case Some(x) => Success(x)
    case None => f
  }

  private val rName = """^(\w+)\s+((.*)\s+)?(\w+)$""".r

  def parse(name: String): Try[Name] = name match {
    case rName(first, _, middle, last) => sequence(MiniDatabase2.map3(Some(first), Some(middle), Some(last)) { case (f, m, l) => Name(f, Option(m), l) }, Failure(new IllegalArgumentException(name)))
    case rName(first, last) => sequence(MiniDatabase2.map3(Some(first), None, Some(last)) { case (f, m, l) => Name(f, m, l) }, Failure(new IllegalArgumentException(name)))
    case _ => Failure(new IllegalArgumentException(name))
  }
}
