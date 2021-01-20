package edu.neu.coe.csye7200.fp

import edu.neu.coe.csye7200.Renderable
import org.scalatest.Inside
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.Try

case class Scalar(s: String) extends Renderable {
  def render: String = s.toUpperCase()
}

/**
  * See https://en.wikipedia.org/wiki/Three-valued_logic#Logics
  *
  * @author scalaprof
  */
class RenderableSpec extends AnyFlatSpec with Matchers with Inside {
  behavior of "Renderable"
  it should "render simple values like toString" in {
    Scalar("x").render shouldBe "X"
  }
  it should "render list values with indentation" in {
    val list = Seq(Scalar("x"), Scalar("y"), Scalar("z"))
    list.render shouldBe "(\nX,\nY,\nZ\n)"
  }
  it should "render list values with double indentation" in {
    val list = Seq(Seq(Scalar("x0"), Scalar("x1")), Seq(Scalar("y0"), Scalar("y1")), Seq(Scalar("z0"), Scalar("z1")))
    list.render shouldBe "(\n(\nX0,\nX1\n),\n(\nY0,\nY1\n),\n(\nZ0,\nZ1\n)\n)"
  }
  it should "render option values" in {
    val xo = Option(Scalar("x"))
    xo.render shouldBe "Some(X)"
  }
  it should "render try values" in {
    val xy = Try(Scalar("x"))
    xy.render shouldBe "Success(X)"
  }
  it should "render either values" in {
    val e = Left(Scalar("x"))
    e.render shouldBe "Left(X)"
  }
}
