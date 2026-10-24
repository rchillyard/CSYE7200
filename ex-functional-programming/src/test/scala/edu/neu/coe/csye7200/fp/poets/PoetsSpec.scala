package edu.neu.coe.csye7200.fp.poets

import edu.neu.coe.csye7200.CancelOnNotImplemented
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * @author scalaprof
 */
class PoetsSpec extends AnyFlatSpec with Matchers with CancelOnNotImplemented {

    "toXML" should "work for Li Bai" in {
    val xml = <poet><name language="en">Li Bai</name><name language="zh">李白</name></poet>
    val liBai = Poet.fromXML(xml)
    val xmlString = liBai.toXML
    pending // TODO get these aligned correctly
    val expected =
      <poet>
        <name language="en">
        Li Bai
      </name> <name language="zh">
        李白
      </name>
      </poet>
      xmlString should equal (expected)
  }

}