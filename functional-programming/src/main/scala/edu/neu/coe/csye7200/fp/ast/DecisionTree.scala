package edu.neu.coe.csye7200.fp.ast

case class DecisionTree(ks: Seq[String]) extends Renderable {

  override def apply(prefix: String, suffix: String): String = {
    def leaf(ws: Seq[String]): String = s""""${ws.mkString}$suffix""""

    def comparisons(k: String, js: Seq[String], last: Boolean): String =
      if (last) ""
      else "if " + ((js map (k + " > " + _)) mkString("(", " && ", ")"))

    def doRenderString(k: String) = {
      val js = ks filterNot (_ == k)
      s""" ${comparisons(k, js, k == ks.last)} """ + DecisionTree(js)(prefix + "  ", k + suffix) + "\n "
    }

    if (ks.size == 2) s"""if (${ks.head}>${ks.last}) ${leaf(ks.reverse)} else ${leaf(ks)}"""
    else (for (k <- ks) yield doRenderString(k)) mkString(" ", " else ", "")
  }
}

trait Renderable extends ((String, String) => String)

object DecisionTree extends App {
  val tree = DecisionTree(Seq("a", "b", "c", "d"))
  val w = tree("", "")
  println(w)
}

case class ASTNode(value: Either[Condition, Leaf])

case class Condition(comparision: Comparison, co: Option[(Logic, Condition)]) {

}

case class Logic(operator: Boolean)

case class Comparison(operator: String, left: String, right: String)

case class Leaf(w: String)
