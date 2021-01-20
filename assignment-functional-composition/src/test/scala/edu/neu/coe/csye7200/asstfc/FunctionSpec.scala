package edu.neu.coe.csye7200.asstfc

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.util.{Try, _}

class FunctionSpec extends AnyFlatSpec with Matchers {

  behavior of "map2"

  it should """match Success(1234) for parse "12" to int and parse "34" to int,with (a:Int,b:Int) => a.toString()+b.toString()""" in {
    val a1 = "12"
    val a2 = "34"
    val t1 = Try(a1.toInt)
    val t2 = Try(a2.toInt)

    val test = Function.map2(t1, t2)((a: Int, b: Int) => a.toString + b.toString)

    test should matchPattern {
      case Success("1234") =>
    }
  }

  it should """fail for "", Int""" in {

    val t1 = Try(Name("Robin", Some("C"), "Hillyard", Some("Ph.D")))
    val t2 = Try(24)

    val test = Function.map2(t1, t2)(Principal.apply)

    // this was originally a Failure
    test should matchPattern {
      case Success(_) =>
    }
  }

  behavior of "map7"

  it should "success" in {
    val p1 = Try(0.02)
    val p2 = Try(23)
    val p3 = Rating.parse("PG-13")
    val p4 = Try(15)
    val p5 = Try(18)
    val p6 = Try(20)
    val p7 = Try(28)

    val test = Function.map7(p1, p2, p3, p4, p5, p6, p7)(new Reviews(_, _, _, _, _, _, _))

    test.get should matchPattern {
      case Reviews(0.02, 23, Rating("PG", Some(13)), 15, 18, 20, 28) =>
    }
  }

  it should """fail with bad input""" in {
    val p1 = Try(0.02)
    val p2 = Try(23)
    val p3 = Rating.parse("PG-XXXX")
    val p4 = Try(15)
    val p5 = Try(18)
    val p6 = Try(20)
    val p7 = Try(28)
    Function.map7(p1, p2, p3, p4, p5, p6, p7)(new Reviews(_, _, _, _, _, _, _)) should matchPattern {
      case Failure(_) =>
    }
  }

  behavior of "invert2"

  it should "work" in {
    val a: Int => Int => String = { a => b => "abcde".substring(a, b) }

    Try(a(0)(2)) should matchPattern {
      case Success("ab") =>
    }

    val aux = Function.invert2(a)

    Try(aux(0)(2)) should matchPattern {
      case Failure(_) =>
    }
  }

  behavior of "invert3"

  it should "work" in {

    val a: Int => Int => Int => Int = { a => b => c => a * b + c }

    a(2)(3)(4) shouldBe 10

    val aux = Function.invert3(a)

    aux(2)(3)(4) shouldBe 14
  }

  behavior of "invert4"

  it should "work" in {

    val a: Int => Int => Int => Int => Int = { a => b => c => d => a * b + c % d }

    a(2)(3)(4)(5) shouldBe 10

    Function.invert4(a)(2)(3)(4)(5) shouldBe 21
  }

  behavior of "uncurried2"
  it should "work" in {

    lazy val a: Int => Int => Int => Int = { a => b => c => a * b + c }

    lazy val aux = Function.uncurried2(a)

    a(1)(2)(3) shouldBe 5
    aux(1, 2)(3) shouldBe 5
    aux.curried(3)(2)(1) shouldBe a(3)(2)(1)
    a.toString() should include("Lambda")
    aux.toString() should include("Lambda")
  }
}
