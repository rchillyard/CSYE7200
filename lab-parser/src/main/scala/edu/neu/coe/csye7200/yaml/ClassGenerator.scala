package edu.neu.coe.csye7200.yaml
//
//import scala.reflect.runtime.{universe => ru}
//import scala.tools.reflect.ToolBox
//
//object ClassGenerator {
//  def main(args: Array[String]): Unit = {
//    val tb = ru.runtimeMirror(getClass.getClassLoader).mkToolBox()
//
//    val classDefinition = """
//      case class CrimeData1(
//        lsoa_code: String,
//        borough: String,
//        major_category: String,
//        minor_category: String,
//        value: Int,
//        year: Int,
//        month: Int
//      )
//
//      CrimeData1
//    """
//
//    val companionObject = tb.eval(tb.parse(classDefinition))
//
//    val instance = companionObject.asInstanceOf[
//            (String, String, String, String, Int, Int, Int) => Any
//    ].apply(
//      "xyz",
//      "some place",
//      "crime1",
//      "crime1_",
//      42,
//      2024,
//      12
//    )
//
//    println(instance)
//  }
//}