package edu.neu.coe.csye7200.assthw

import edu.neu.coe.csye7200.assthw.Gender.{female, male}
import edu.neu.coe.csye7200.assthw.HappyFamilies.maybeAge
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.util.{Failure, Success}

class HappyFamiliesSpec extends AnyFlatSpec with should.Matchers {
  private val bentley = Person("Bentley", male, Some(13))
  private val gingerSnap = Person("GingerSnap", female, Some(12))
  private val xena = Person("Xena", Gender(None), Some(7))

  behavior of "HappyFamilies"

  it should "maybeAge" in {
    maybeAge("1") shouldBe Some(1)
    maybeAge("") shouldBe None
  }

  it should "Person" in {
    bentley.toString shouldBe "Person(Bentley,M,Some(13))"
  }

  it should "gender" in {
    Gender(Some(true)).toString shouldBe "M"
    Gender(Some(false)).toString shouldBe "F"
    Gender(None).toString shouldBe "-"
  }

  it should "join" in {
    male.join(female) shouldBe Some(MaleFemale)
    female.join(male) shouldBe Some(FemaleMale)
    female.join(female) shouldBe Some(Illegal)
    male.join(male) shouldBe Some(Illegal)
    Gender(None).join(male) shouldBe None
    Gender(None).join(female) shouldBe None
    male join Gender(None) shouldBe None
    female join Gender(None) shouldBe None
  }

  it should "marry" in {
    bentley.marry(gingerSnap) shouldBe Success(Family(bentley, gingerSnap, Nil))
    bentley.marry(bentley) should matchPattern { case Failure(_: GenderException) => }
    bentley.marry(xena) should matchPattern { case Failure(_: NoSuchElementException) => }
  }

  it should "birth" in {
    val f1 = for (f <- bentley.marry(gingerSnap)) yield f.birth(xena)
    f1 shouldBe Success(Family(bentley, gingerSnap, Seq(xena)))
  }

}
