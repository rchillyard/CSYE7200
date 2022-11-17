package edu.neu.coe.csye7200.fp

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Try}

class DebugFSpec extends AnyFlatSpec with should.Matchers {

    import DebugF._

    behavior of "!!"

    it should "write debug info to System.out" in {
        import Writable.SysOutWritable
        ("x" !! 1) shouldBe 1
    }

    it should "write debug info to StringBuilder" in {
        import Writable.StringBuilderWritable
        ("x" !! 1) shouldBe 1
        implicitly[Writable].readBack shouldBe "DebugF: x: 1\n"
        implicitly[Writable].close()
    }

    ignore should "write debug info to LogFile" in {
        import Writable.LogFileWritable
        ("x" !! 1) shouldBe 1
        implicitly[Writable].readBack shouldBe "DebugF: x: 1"
        ("xs" !! List(1, 2, 3)) shouldBe List(1, 2, 3)
        implicitly[Writable].readBack shouldBe "DebugF: xs: List(1, 2, 3)"
        implicitly[Writable].close()
    }

    it should "write more debug info to StringBuilder" in {
        import Writable.StringBuilderWritable
        ("x" !! 1) shouldBe 1
        ("xs" !! List(1, 2, 3)) shouldBe List(1, 2, 3)
        ("xo" !! Option(1)) shouldBe Some(1)
        ("xy" !! Try(1 / 0)) should matchPattern { case Failure(_) => }
        ("t" !! (math.Pi, math.E)) shouldBe(math.Pi, math.E)
        implicitly[Writable].readBack shouldBe "DebugF: x: 1\nDebugF: xs: List(1, 2, 3)\nDebugF: xo: Some(1)\nDebugF: xy: Failure(java.lang.ArithmeticException: / by zero)\nDebugF: t: (3.141592653589793,2.718281828459045)\n"
        implicitly[Writable].close()
    }

    behavior of "!|"

    it should "not write debug info to System.out" in {
        import Writable.SysOutWritable
        ("x" !| 1) shouldBe 1
    }

    it should "not write debug info to StringBuilder" in {
        import Writable.StringBuilderWritable
        ("x" !| 1) shouldBe 1
        implicitly[Writable].readBack shouldBe ""
    }

    ignore should "not write debug info to LogFile" in {
        import Writable.LogFileWritable
        ("x" !| 1) shouldBe 1
        a[Exception] shouldBe thrownBy(implicitly[Writable].readBack)
    }

}
