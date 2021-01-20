package edu.neu.coe.csye7200

import scala.util._

/**
  * Created by scalaprof on 3/14/17.
  */
trait Renderable {
  def render: String
}

case class RenderableTraversable(xs: Iterable[_]) extends Renderable {
  def render: String = {
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = {
      var first = true
      b append start
      for (x <- xs) {
        if (first) {
          b append Renderable.renderElem(x)
          first = false
        }
        else {
          b append sep
          b append Renderable.renderElem(x)
        }
      }
      b append end

      b
    }

    addString(new StringBuilder, "(\n", ",\n", "\n" + ")").toString()
  }
}

case class RenderableOption(xo: Option[_]) extends Renderable {
  def render: String = xo match {
    case Some(x) => s"Some(" + Renderable.renderElem(x) + ")"
    case _ => "None"
  }
}

case class RenderableTry(xy: Try[_]) extends Renderable {
  def render: String = xy match {
    case Success(x) => s"Success(" + Renderable.renderElem(x) + ")"
    case _ => "Failure"
  }
}

case class RenderableEither(e: Either[_, _]) extends Renderable {
  def render: String = e match {
    case Left(x) => s"Left(" + Renderable.renderElem(x) + ")"
    case Right(x) => s"Right(" + Renderable.renderElem(x) + ")"
  }
}

object Renderable {

  import scala.language.implicitConversions

  implicit def renderableTraversable(xs: Iterable[_]): Renderable = RenderableTraversable(xs)

  implicit def renderableOption(xo: Option[_]): Renderable = RenderableOption(xo)

  implicit def renderableTry(xy: Try[_]): Renderable = RenderableTry(xy)

  implicit def renderableEither(e: Either[_, _]): Renderable = RenderableEither(e)

  def renderElem(elem: Any): String = elem match {
    case xo: Option[_] => renderableOption(xo).render
    case xs: Iterable[_] => renderableTraversable(xs).render
    case xy: Try[_] => renderableTry(xy).render
    case e: Either[_, _] => renderableEither(e).render
    case r: Renderable => r.render
    case x => x.toString
  }
}

///**
//  * Bad class -- not needed
//  * @param as
//  * @tparam A
//  */
//case class RenderableSeq[A <: Renderable](as: Seq[A]) extends Renderable {
//  def render: String = as map (_ render) mkString ","
//}
