/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.fp.parse

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SubstringSpec extends AnyFlatSpec with Matchers {

  import Substring._

  behavior of "substring"

  private val alphabet = "abcdefg"

  it should "yield true for empty prefix" in {
    substring("", "") shouldBe true
    substring("", alphabet) shouldBe true
  }
  it should "yield false for empty string" in {
    substring(alphabet, "") shouldBe false
  }
  it should "yield true for equal strings" in {
    substring(alphabet, alphabet) shouldBe true
  }
  it should "yield true for prefix strings" in {
    substring("a", alphabet) shouldBe true
    substring("ab", alphabet) shouldBe true
    substring("abc", alphabet) shouldBe true
    substring("abcd", alphabet) shouldBe true
    substring("abcde", alphabet) shouldBe true
    substring("abcdef", alphabet) shouldBe true
  }
  it should "yield true for internal strings" in {
    substring("cde", alphabet) shouldBe true
    substring("bcd", alphabet) shouldBe true
    substring("def", alphabet) shouldBe true
  }
  it should "yield false for non-matching internal strings" in {
    substring("cfe", alphabet) shouldBe false
    substring("bad", alphabet) shouldBe false
    substring("dgf", alphabet) shouldBe false
  }
  it should "yield true for suffix strings" in {
    substring("bcdefg", alphabet) shouldBe true
    substring("cdefg", alphabet) shouldBe true
    substring("defg", alphabet) shouldBe true
    substring("efg", alphabet) shouldBe true
    substring("fg", alphabet) shouldBe true
    substring("g", alphabet) shouldBe true
  }
  it should "yield false for no sub-strings" in {
    substring("xyz", alphabet) shouldBe false
    substring("yz", alphabet) shouldBe false
    substring("z", alphabet) shouldBe false
    substring("0", alphabet) shouldBe false
  }
  it should "yield false for special characters" in {
    substring("\n", alphabet) shouldBe false
    substring("\t", alphabet) shouldBe false
    substring(" ", alphabet) shouldBe false
  }
}
