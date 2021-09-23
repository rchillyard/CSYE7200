package edu.neu.coe.csye7200.fp.parse

import scala.annotation.tailrec

object Substring {

  /**
    * Method to determine if <code>sub</code> is a substring of <code>string</code>.
    *
    * @param sub    the candidate substring.
    * @param string the full string.
    * @return true if <code>sub</code> is a substring of <code>string</code>.
    */
  def substring(sub: String, string: String): Boolean = {
    val p = sub.toList

    /**
      * Tail-recursive method to determine if <code>p</code> is a subsequence of <code>s</code>
      *
      * @param s the super-sequence to be tested (part of the original "string").
      * @return as follows:
      *         (1) <code>p</code> longer than <code>s</code> => false;
      *         (2) <code>p</code> elements match the corresponding <code>s</code> elements (starting at the start of <code>s</code>) => true
      *         (3) recursively invoke substring on <code>p</code> and the tail of <code>s</code>.
      */
    @tailrec def substring(s: Seq[Char]): Boolean = p.length <= s.length && (
//      s.startsWith(p) || (
            // TODO implement the alternative as follows:
            startsWith(s, p) || (
                    s match {
                      case Nil => false
                      case _ :: z => substring(z)
                    }
                    )
            )

    p.isEmpty || substring(string.toList)
  }

  private def startsWith(s: Seq[Char], p: Seq[Char]): Boolean = ??? // TO BE IMPLEMENTED
}
