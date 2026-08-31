package edu.neu.coe.csye7200

package object concordance {

  type Concordance = Map[String, IndexedSeq[(Int, IndexedSeq[(Int, Seq[(Int, Int, String)])])]]
}
