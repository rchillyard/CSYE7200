package edu.neu.coe.csye7200.poets

import org.scalatest.{ FlatSpec, Matchers }

/**
 * @author scalaprof
 */
class PoetsSpec extends FlatSpec with Matchers {
  
  "toXML" should "work for Li Bai" in {
    val xml = <poet><name language="en">Li Bai</name><name language="zh">李白</name></poet>
    val liBai = Poet.fromXML(xml)
    val xmlString = liBai.toXML
    xmlString should equal (<poet><name language="en">Li Bai</name><name language="zh">李白</name></poet>)
  }

}
