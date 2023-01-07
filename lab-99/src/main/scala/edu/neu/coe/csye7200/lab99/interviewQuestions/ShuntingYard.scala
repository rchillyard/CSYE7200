package edu.neu.coe.csye7200.lab99.interviewQuestions

import scala.util.matching.Regex

object ShuntingYard {

  val infixR: Regex = """(([\s()]*)(\d+|[*+]))+""".r

  def parseString(s: String): List[String] = infixR.unapplySeq(s).getOrElse(List("<no match>"))


}
