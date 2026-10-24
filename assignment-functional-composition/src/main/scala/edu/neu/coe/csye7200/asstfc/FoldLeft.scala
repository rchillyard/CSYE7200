package edu.neu.coe.csye7200.asstfc

object FoldLeft {

  def foldLeft[X, Y](xs: IndexedSeq[X])(y: Y)(f: (Y, X) => Y): Y = {

    val l: Int = 0

    @scala.annotation.tailrec
    def fold(curIndex: Int)(y: Y)(f: (Y, X) => Y): Y = {

      val xo: Option[X] = xs.lift(curIndex)

      xo match {
        case Some(x) => f(y, x)
        case None =>
      }
      fold(l + 1)(y)(f)

    }

    fold(l)(y)(f)

  }

}
