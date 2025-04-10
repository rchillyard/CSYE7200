package edu.neu.coe.csye7200.yaml

//import scala.reflect.runtime.universe.{ClassDef, Quasiquote}
//import scala.reflect.runtime.{universe => ru}
//import scala.tools.reflect.ToolBox

//object ClassGeneratorQ {
//  def main(args: Array[String]): Unit = {
//    val tb = ru.runtimeMirror(getClass.getClassLoader).mkToolBox()
//
//    val classDefinition = q"""
//      case class CrimeData1(
//        lsoa_code: String,
//        borough: String,
//        major_category: String,
//        minor_category: String,
//        value: Int,
//        year: Int,
//        month: Int
//      )
//    """
//
//    val myClass: ClassDef = classDefinition.asInstanceOf[ClassDef]
//    val definedClass: tb.u.Symbol = tb.define(myClass)
//
//    type XXX = Option[definedClass.type ]
//
//    val xxx: XXX =
//
//    val companion: tb.u.Symbol = definedClass.companion
//
//    companion.type.
////    val instance = companionObject.asInstanceOf[
////            (String, String, String, String, Int, Int, Int) => Any
////    ].apply(
////      "xyz",
////      "some place",
////      "crime1",
////      "crime1_",
////      42,
////      2024,
////      12
////    )
//
//    println(companion)
//  }
//}