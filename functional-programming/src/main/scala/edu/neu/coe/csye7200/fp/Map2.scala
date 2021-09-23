package edu.neu.coe.csye7200

/**
  * @author scalaprof
  */
object Map2 {

  //	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  //	  a match {
  //			case Some(aa) => b match {
  //			case Some(bb) => Some(f(aa,bb))
  //			case _ => None
  //		}
  //		case _ => None
  //	}

  def map[B, C](b: Option[B])(f: B => C): Option[C] =
    b match {
      case Some(bb) => Some(f(bb))
      case _ => None
    }

  def flatMap[A, B](a: Option[A])(f: A => Option[B]): Option[B] =
    a match {
      case Some(aa) => f(aa)
      case _ => None
    }

  def map2b[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    flatMap(a)(aa => map(b)(bb => f(aa, bb)))

  def map2a[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] =
    for {
      aa <- a
      bb <- b
      cc <- c
    } yield f(aa, bb, cc)

  def map2right[A, B, C, S](a: Either[S, A], b: Either[S, B])(f: (A, B) => C): Either[S, C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def map2leftRight[A, B, C, S](a: Either[S, A], b: Either[S, B])(fr: (A, B) => C)(fl: (S, S) => S): Either[S, C] =
    (a, b) match {
      case (Left(aa), Left(bb)) => Left(fl(aa, bb))
      case _ => map2right(a, b)(fr)
    }
}

object ReadURL {

  import java.net.URL
  import scala.io.Source
  import scala.util._

  def getURLContent(url: String): Try[Iterator[String]] =
    for {
      url <- Try(new URL(url))
      connection <- Try(url.openConnection())
      is <- Try(connection.getInputStream)
      source = Source.fromInputStream(is)
    } yield source.getLines()

  def wget(args: Array[String]): Unit = {
    val maybePages = for {
      arg <- args
      x = getURLContent(arg)
    } yield x
    for {
      Success(p) <- maybePages
      l <- p
    } println(l)
  }

  def main(args: Array[String]): Unit = {
    println(s"web reader: ${args.toList}")
    wget(args)

    val ok: Either[String, Int] = Right(1)
    val notOk: Either[String, Int] = Left("bad")
    val result = Map2.map2right(ok, notOk) {
      _ + _
    }
    print(result)
  }
}
