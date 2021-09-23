package edu.neu.coe.csye7200

import edu.neu.coe.csye7200.TypeClasses.TypeClass2Int

object TypeClasses extends App {

  trait TypeClass2Int[T] extends TypeClass2[T, Int] {
    def tToU(t: T): Int
  }

  implicit object TypeClass2StringInt$ extends TypeClass2Int[String] {
    def tToU(t: String): Int = t.toInt
  }

  //  implicit object TypeClass2StringInt extends TypeClass2[String, Int] {
  //    def tToU(t: String): Int = t.toInt
  //  }

  val y = Y[String, Int]()

  def intToT[T: Numeric](x: Int): T = implicitly[Numeric[T]].fromInt(x)

  def showIntAsT[T: Numeric : Show](x: Int): String = implicitly[Show[T]].show(implicitly[Numeric[T]].fromInt(x))

  //  def intToT[T](x: Int)(implicit ev: Numeric[T]): T = implicitly[Numeric[T]].fromInt(x)
}

trait Show[T] {
  def show(t: T): String
}

trait TypeClass[T] {

}

trait TypeClass2[T, U] {
  def tToU(t: T): U
}

case class X[T: TypeClass2Int]() {
  def z(t: T): Int = implicitly[TypeClass2Int[T]].tToU(t)
}

case class Y[T, U]()(implicit ev: TypeClass2[T, U]) {
  def z(t: T): U = ev.tToU(t)
}

case class Z[T: Numeric, U]()(implicit ev: TypeClass2[T, U]) {
  def z(x: Int): U = ev.tToU(implicitly[Numeric[T]].fromInt(x))
}
