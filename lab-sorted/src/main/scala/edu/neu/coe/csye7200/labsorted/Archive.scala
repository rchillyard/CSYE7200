package edu.neu.coe.csye7200.labsorted

/**
 * Data structure to represent an Archive in memory.
 * It consists of two parts: the archive and the journal (see params below).
 * Keep in mind that <code>Ordering[T]</code> might only consider a "key" derived from <code>T</code>, rather than the entire object.
 *
 * @param archive the ordered, indexed, sequence representing the archive at the start of the day.
 * @param journal the unordered sequence of elements which have been added this day.
 * @tparam T the underlying type of the elements.
 *           Must provide implicit evidence of <code>Ordering[T]</code>.
 */
case class Archive[T: Ordering](archive: IndexedSeq[T], journal: Seq[T]) {

  /**
   * Insert a <code>T</code> value into the journal.
   *
   * @param t the <code>T</code value.
   * @return a new <code>Archive[T]</code> which includes all the old entries together with <code>t</code>.
   */
  def insert(t: T): Archive[T] =
  // TO BE IMPLEMENTED  implement me. [7]
    ???

  /**
   * Search the archive for the element <code>t</code>.
   *
   * @param t the element to find.
   * @return the (optional) element that was found.
   */
  def search(t: T): Option[T] = {
    // TO BE IMPLEMENTED  implement me as described above (total points: 17)
        ???
  }

  /**
   * Method to perform a binary search on <code>archive</code>.
   *
   * @param t the value to find.
   * @return the (optional) index of the found value.
   */
  private def binarySearch(t: T): Option[Int] = Archive.binarySearch(t, archive)(0, archive.size)
}

object Archive {
  /**
   * Construct a new instance of <code>Archive[P]</code> with the sorted version of the given value of
   * <code>archive</code> and an empty sequence for the journal.
   *
   * @param archive An instance of <code>Seq[P]</code> which you must ordered and index before passing into the constructor.
   * @tparam P the underlying type of the elements.
   *           Must provide implicit evidence of <code>Ordering[P]</code>.
   * @return a newly constructed instance of <code>Archive[P]</code>.
   */
  def apply[P: Ordering](archive: Seq[P]): Archive[P] =
  // TO BE IMPLEMENTED  implement me [8]
    ???

  /**
   * Method to perform a binary search on a sorted, indexed sequence.
   * Note that this method is not tail-recursive on account of its interaction with <code>flatWhen</code>.
   * But the depth of recursion is O(lg n) where n is the number of elements in the archive and therefore
   * tail-recursion is not essential.
   * You could easily make it rail-recursive by inlining the call to flatWhen (but you don't need to do that for the Final Exam Spring 2024).
   *
   * @param p    the value to find.
   * @param ps   a sorted, indexed sequence.
   * @param from the start index.
   * @param to   the end index (this points to the first element to be ignored).
   * @return the (optional) index of the found value.
   */
  private def binarySearch[P: Ordering](p: P, ps: IndexedSeq[P])(from: Int, to: Int): Option[Int] = flatWhen(from < to) {
    val bs: (Int, Int) => Option[Int] =
    // TO BE IMPLEMENTED  implement an expression for bs [5]
        ???
    val po: Ordering[P] =
    // TO BE IMPLEMENTED  implement me [3]
        ???
    val mid = (from + to) / 2
    po.compare(p, ps(mid)) match {
      case 0 => Some(mid)
      case x if x > 0 =>
        // TO BE IMPLEMENTED  implement me by using bs appropriately. [3]
            ???
      case _ =>
        // TO BE IMPLEMENTED  implement me by using bs appropriately. [3]
            ???
    }
  }

  /**
   * This method is to <code>Option.when</code> as <code>flatMap</code> is to <code>map</code>.
   * It is essentially a way to widen a <code>Boolean</code> into an <code>Option[P]</code>.
   *
   * @param condition if true, then we evaluate and return <code>po</code>.
   * @param po        an <code>Option[P]</code>.
   * @tparam P the underlying type of the result.
   * @return an <code>Option[P]</code> which will be <code>None</code> if the condition is <code>false</code>, otherwise <code>po</code>.
   */
  def flatWhen[P](condition: Boolean)(po: => Option[P]): Option[P] =
    // TO BE IMPLEMENTED  implement flatWhen. [7]
    ???
}