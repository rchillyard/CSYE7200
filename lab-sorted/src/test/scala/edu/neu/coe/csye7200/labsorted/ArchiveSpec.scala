package edu.neu.coe.csye7200.labsorted

import edu.neu.coe.csye7200.labsorted.Archive.flatWhen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class ArchiveSpec extends AnyFlatSpec with should.Matchers {

  behavior of "Archive class"

  private val list = List(7, 3, 4, 0, 6, 9)

  it should "insert" in {
    val archive0 = Archive(list)
    val archive1 = archive0.insert(5)
    archive1.archive shouldBe List(0, 3, 4, 6, 7, 9)
    archive1.journal shouldBe List(5)
  }

  it should "search 1" in {
    val archive = Archive(list)
    archive.search(3) shouldBe Some(3)
    archive.search(0) shouldBe Some(0)
    archive.search(9) shouldBe Some(9)
    archive.search(5) shouldBe None
  }

  it should "search 2" in {
    val archive0 = Archive(list)
    archive0.search(3) shouldBe Some(3)
    archive0.search(5) shouldBe None
    val archive1 = archive0.insert(5)
    archive1.search(3) shouldBe Some(3)
    archive1.search(5) shouldBe Some(5)
  }

  behavior of "Archive object"

  it should "apply" in {
    val archive = Archive(list)
    archive.archive shouldBe List(0, 3, 4, 6, 7, 9)
    archive.journal shouldBe Nil
  }

  it should "flatWhen true" in {
    val sb = new StringBuilder()
    flatWhen(condition = true) {
      sb.append("X"); Some(1)
    } shouldBe Some(1)
    sb.toString() shouldBe "X"
  }

  it should "flatWhen false" in {
    val sb = new StringBuilder()
    flatWhen(condition = false) {
      sb.append("X"); Some(1)
    } shouldBe None
    sb.isEmpty shouldBe true
  }
}
