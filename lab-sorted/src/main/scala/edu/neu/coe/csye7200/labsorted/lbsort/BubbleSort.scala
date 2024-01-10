package edu.neu.coe.csye7200.labsorted.lbsort

import edu.neu.coe.csye7200.sort.BubbleSortJava
import scala.reflect.ClassTag
import scala.util.Random

/**
  * Case class to perform BubbleSort on a linked list.
  *
  * @param xo the evidence for Ordering[X].
  * @tparam X the underlying type that this class knows how to sort.
  */
case class BubbleSort[X]()(implicit xo: Ordering[X]) {

  /**
    * Mutating method to sort a list of Xs defined by the head element of type Element[X].
    *
    * NOTE: this is not idiomatic Scala code: it contains null, var, do/while, break, etc.
    * NOTE: we also use eq to addresses rather than values.
    *
    * @param head the head Element of a linked list of Xs.
    */
  def sortList(head: Element[X]): Unit = {
    import scala.util.control.Breaks.{break, breakable}
    var sorted: Element[X] = null
    breakable(
      do {
        val stopped -> swapped = pass(head, sorted)
        sorted = stopped
        if (!swapped) break
      } while (!(sorted eq head))
    )
  }

  /**
    * Method which conditionally swaps the values of two Elements: x and y.
    * If x and y are initially in their correct order (not inverted), then no action is taken.
    *
    * @param x an Element[X].
    * @param y an Element[X].
    * @return true if a swap actually happened.
    */
  private def swapConditional(x: Element[X], y: Element[X]) = {
    val result = xo.compare(x.x, y.x) < 0
    if (result) {
      val temp = x.x
      x.x = y.x
      y.x = temp
    }
    result
  }

  /**
    * Method to make one pass through a linked list, keeping two Elements x and y which are conditionally swapped.
    *
    * @param start the Element[X] at which to start the pass.
    * @param stop  the Element[X] at which to stop.
    * @return a Tuple consisting of the last value of x and a Boolean representing whether this pass swapped anything.
    */
  private def pass(start: Element[X], stop: Element[X]): (Element[X], Boolean) = {
    var x = start
    var yo = x.next
    var swapped = false
    while (yo.isDefined && !(yo.get eq stop)) {
      swapped |= swapConditional(x, yo.get)
      x = yo.get
      yo = x.next
    }
    x -> swapped
  }
}

/**
  * Companion object to BubbleSort.
  */
object BubbleSort {
  /**
    * Method to sort a Seq[X] using BubbleSort.
    * This method does not mutate its input.
    *
    * @param xs a Seq[X].
    * @tparam X the underlying type of xs, must support Ordering.
    * @return a new Seq[X].
    */
  def sort[X: Ordering](xs: Seq[X]): Seq[X] = Element.create(xs) match {
    case None =>
      Nil
    case Some(xe) =>
      BubbleSort().sortList(xe)
      Element.toRevSeq(xe)
  }
//
//  def sortByJava[X: Ordering: ClassTag](xs: Seq[X]): Seq[X] = {
//    val sorter = new BubbleSortJava[X](implicitly[Ordering[X]])
//    import scala.language.implicitConversions
//
//    val xs1: Array[X] = xs.toArray
//    sorter.sort(xs1)
//    xs1 to List
//  }

}

/**
  * Mutating case class representing an element of type X.
  *
  * @param x    (variable) value of type X.
  * @param next optional pointer to the next Element[X].
  * @tparam X the underlying type of the element.
  */
case class Element[X](var x: X, next: Option[Element[X]])

/**
  * Companion object to case class Element[X].
  */
object Element {
  import scala.annotation.tailrec

  /**
    * Method to create a linked list of Xs from a Seq[X].
    *
    * @param xs the elements of type X.
    * @tparam X the underlying type of the input and the result.
    * @return an optional Element[X] (will be None if xs is empty).
    */
  def create[X](xs: Seq[X]): Option[Element[X]] = {
    @tailrec
    def inner(result: Option[Element[X]], _xs: Seq[X]): Option[Element[X]] = {
    _xs match {
      case Nil => result
      case h :: t => inner(Some(Element(h, result)), t)
    }
    }
    inner(None, xs.reverse)
  }

  /**
    * Method to create a Seq[X] from a linked list of Element[X].
    *
    * @param head an Element[X] representing the head of a list of elements.
    * @tparam X the underlying type of the input and result.
    * @return a Seq[X].
    */
  def toRevSeq[X](head: Element[X]): Seq[X] = {
    @tailrec
    def inner(xs: Seq[X], xeo: Option[Element[X]]): Seq[X] = xeo match {
      case None => xs
      case Some(xe) => inner(xe.x +: xs, xe.next)
    }

    inner(Nil, Some(head))
  }
}

object BenchmarkBubbleSort extends App {

  val random = new Random()

  def doSort(n: Int) {
    val xs: Seq[Int] = LazyList.continually(random.nextInt()) take n to List


    val result: Seq[Int] = BubbleSort.sort(xs)
//    println(result)
  }

  doSort(100000)
}