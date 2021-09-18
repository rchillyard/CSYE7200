package edu.neu.coe.csye7200.fp.util

/**
  * Created by Bowen Cai on 1/24/2015.
  *
  * Lifted and edited by ScalaProf from http://stackoverflow.com/questions/11305290/is-there-way-to-create-tuple-from-listwithout-codegeneration
  */
sealed trait Product0 extends Any with Product {
  def productArity = 0

  def productElement(n: Int) = throw new IllegalStateException("No element")

  def canEqual(that: Any) = false
}

object Tuple0 extends Product0 {
  override def toString = "()"
}

case class SeqProduct(elems: Any*) extends Product {
  override def productArity: Int = elems.size

  override def productElement(i: Int): Any = elems(i)

  override def toString: String = elems.addString(new StringBuilder(elems.size * 8 + 10), "(", ",", ")").toString()
}

object Tuples {
  private[this] val ctors = {
    val ab = Array.newBuilder[java.lang.reflect.Constructor[_]]
    for (i <- 1 to 22) {
      val tupleClass = Class.forName("scala.Tuple" + i)
      ab += tupleClass.getConstructors.apply(0)
    }
    ab.result()
  }

  def toTuple(elems: Seq[Any]): Product = elems.length match {
    case 0 => Tuple0
    case size if size <= 22 =>
      val refs = for (e <- elems) yield e.asInstanceOf[AnyRef]
      ctors(size - 1).newInstance(refs: _*).asInstanceOf[Product]
    case size if size > 22 => SeqProduct(elems: _*)
  }
}
