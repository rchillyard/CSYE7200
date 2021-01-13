/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.asstll

import scala.annotation.tailrec

/**
  * Case class representing a non-empty ListLike.
  *
  * @param x        the head of the stream.
  * @param lazyTail a function which, when invoked, yields the tail of this stream.
  * @tparam X the underlying type of the stream, and of the value <code>x</code>.
  */
case class MyLazyList[X](x: X, lazyTail: () => ListLike[X]) extends LazyListLike[X] {


  /**
    * Concatenate this ListLike with ys.
    *
    * @param ys the stream to be used if/when this stream is exhausted.
    * @tparam Y the underlying type of ys and the result.
    * @return a <code>ListLike[Y]<code> which contains all the elements of this followed by all the elements of ys.
    */
  def ++[Y >: X](ys: ListLike[Y]): ListLike[Y] = MyLazyList[Y](x, () => lazyTail() ++ ys)

  def head: X = x

  def tail: ListLike[X] = lazyTail()

  /**
    * The "flatMap" function.
    *
    * @param f a function which converts an <code>X<code into a <code>ListLike[Y]<code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>ListLike[Y]<code> where each element is the result of applying <code>f<code to the corresponding
    *         element of <code>this<code> and then "flattening" the result
    *         by concatenating all streams together.
    */
  def flatMap[Y](f: X => Monadic[Y]): ListLike[Y] = {
    val y = f(x).asInstanceOf[ListLike[Y]]
    MyLazyList(y.head, () => y.tail ++ lazyTail().flatMap(f))
  }

  /**
    * Method to determine if this ListLike object is empty.
    *
    * @return true if the list is empty.
    */
  def isEmpty: Boolean = false

  /**
    * Method to form a new list-like object by pre-pending y
    *
    * @param y the value to serve as the new head
    * @tparam Y the type of y
    * @return a new <code>ListLike[Y]<code> object with y as its head and this as its tail
    */
  def +:[Y >: X](y: Y): ListLike[Y] = MyLazyList(y, () => this)

  /**
    * The "filter" function.
    *
    * @param p a predicate which takes an <code>X<code> and yields a <code>Boolean<code>.
    * @return a <code>Monadic[X]<code> where every element satisfies the predicate <code>p<code>.
    */
  def filter(p: X => Boolean): ListLike[X] = {
    val tailFunc = () => lazyTail().filter(p)
    if (p(x)) MyLazyList(x, tailFunc) else tailFunc()
  }

  /**
    * Method to "zip" to ListLike objects together
    *
    * @param ys the stream of Ys
    * @tparam Y the underlying type of <code>ys</code>
    * @return a <code>ListLike[(X,Y)]<code> where each element is a tuple of the corresponding elements from this
    *         and ys respectively.
    */
  def zip[Y](ys: ListLike[Y]): ListLike[(X, Y)] = ys match {
    case MyLazyList(y, g) => MyLazyList((x, y), () => lazyTail() zip g())
    case _ => EmptyList
  }

  /**
    * Take the first <code>n<code> elements from this ListLike as a ListLike.
    * NOTE: that this is not tail-recursive so may fail on large lists.
    *
    * @param n the number of elements to take (must not be negative).
    * @return a sequence of length n.
    */
  def take(n: Int): ListLike[X] =
    if (n == 0)
      EmptyList
    else if (n > 0)
      this match {
        case MyLazyList(h, f) => MyLazyList(h, () => f() take n - 1)
        case _ => EmptyList
      }
    else
      throw LazyListException("cannot take negative number of elements")


  /**
    * Drop the first n elements of this ListLike object and return the remainder as a ListLike object.
    *
    * @param n the number of elements to drop (must not be negative).
    * @return a <code>ListLike[(X,Y)]<code> which is shorter than this by n.
    */
  def drop(n: Int): ListLike[X] = n match {
    case 0 => this
    case _ =>
      if (n > 0)
        this match {
          case MyLazyList(_, f) => f().drop(n - 1)
          case _ => EmptyList
        }
      else
        throw LazyListException("cannot drop negative number of elements")
  }

  /**
    * Convert this <code>ListLike[X]</code> into a <code>Seq[X]</code>, element for element.
    *
    * @return a <code>Seq[X]</code>
    */
  def toSeq: Seq[X] = {
    @tailrec
    def inner(rs: Seq[X], xs: ListLike[X]): Seq[X] = xs match {
      case MyLazyList(h, f) => inner(rs :+ h, f())
      case _ => rs
    }

    inner(Nil, this)
  }

  /**
    * Method to yield the size of this list-like object.
    *
    * @return Some(n) where n is size if it's definite; if size is not known (lazy) then return None
    */
  def size(): Option[Int] = None

  /**
    * The "filterNot" function.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filterNot(p: X => Boolean): ListLike[X] = filter(FunctionalUtility.invert(p))

  /**
    * Construct a <code>ListLike[X]</code> with the elements of <code>ys</code>.
    *
    * Note that this is an instance method.
    *
    * @param ys the sequence of elements with which to construct the list-like object.
    * @tparam Y the underlying type of <code>ys</code> and the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def build[Y](ys: Seq[Y]): ListLike[Y] = ys match {
    case h :: t => MyLazyList(h, t)
    case _ => MyLazyList(ys)
  }

  override def iterator: Iterator[X] = Iterator.unfold[X, ListLike[X]](this) {
    case EmptyList => None
    case MyLazyList(x, f) => Some(x -> f())
  }
}

/**
  * Case object representing an empty ListLike.
  */
case object EmptyList extends LazyListLike[Nothing] {

  /**
    * Method to indicate whether it's possible to cheaply calculate the length of this object.
    *
    * @return -1
    */
  override def knownSize: Int = -1

  def head = throw LazyListException("empty")

  def tail: ListLike[Nothing] = EmptyList

  /**
    * The "flatMap" function.
    *
    * @param f a function which converts an <code>X<code into a <code>ListLike[Y]<code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>ListLike[Y]<code> where each element is the result of applying <code>f<code to the corresponding
    *         element of <code>this<code> and then "flattening" the result
    *         by concatenating all streams together.
    */
  def flatMap[Y](f: Nothing => Monadic[Y]): ListLike[Y] = EmptyList

  /**
    * Method to determine if this ListLike object is empty.
    *
    * @return true if the list is empty.
    */
  def isEmpty: Boolean = true

  /**
    * Method to form a new list-like object by pre-pending y
    *
    * @param y the value to serve as the new head
    * @tparam Y the type of y
    * @return a new <code>ListLike[Y]<code> object with y as its head and this as its tail
    */
  def +:[Y >: Nothing](y: Y): ListLike[Y] = unit(y)

  /**
    * Concatenate this ListLike with ys.
    *
    * CONSIDER renaming as ++
    * CONSIDER moving to LazyListLike
    *
    * @param ys the stream to be used if/when this stream is exhausted.
    * @tparam Y the underlying type of ys and the result.
    * @return a <code>ListLike[Y]<code> which contains all the elements of this followed by all the elements of ys.
    */
  def ++[Y >: Nothing](ys: ListLike[Y]): ListLike[Y] = ys

  /**
    * Method to "zip" to ListLike objects together
    *
    * @param ys the stream of Ys
    * @tparam Y the underlying type of <code>ys</code>
    * @return a <code>ListLike[(X,Y)]<code> where each element is a tuple of the corresponding elements from this
    *         and ys respectively.
    */
  def zip[Y](ys: ListLike[Y]): ListLike[(Nothing, Y)] = EmptyList

  /**
    * Convert this ListLike into a <code>Seq[X]</code> by evaluating the first <code>n<code elements and discarding the
    * rest.
    * NOTE: that this is unlike the take method of Stream, which returns a Stream.
    *
    * @param n the number of elements to take (must not be negative).
    * @return a sequence of length n.
    */
  def take(n: Int): ListLike[Nothing] = EmptyList

  /**
    * The "filter" function.
    *
    * @param p a predicate which takes an <code>X<code> and yields a <code>Boolean<code>.
    * @return a <code>Monadic[X]<code> where every element satisfies the predicate <code>p<code>.
    */
  def filter(p: Nothing => Boolean): ListLike[Nothing] = EmptyList

  /**
    * Drop the first n elements of this ListLike object and return the remainder as a ListLike object.
    *
    * @param n the number of elements to drop (must not be negative).
    * @return a <code>ListLike[(X,Y)]<code> which is shorter than this by n.
    */
  def drop(n: Int): ListLike[Nothing] = EmptyList

  /**
    * Convert this <code>ListLike[X]</code> into a <code>Seq[X]</code>, element for element.
    *
    * @return a <code>Seq[X]</code>
    */
  def toSeq: Seq[Nothing] = Nil

  /**
    * Method to yield the size of this list-like object.
    *
    * @return Some(n) where n is size if it's definite; if size is not known (lazy) then return None
    */
  def size(): Option[Int] = Some(0)

  /**
    * The "filterNot" function.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filterNot(p: Nothing => Boolean): ListLike[Nothing] = EmptyList

  /**
    * Construct a <code>ListLike[X]</code> with the elements of <code>ys</code>.
    *
    * Note that this is an instance method.
    *
    * @param ys the sequence of elements with which to construct the list-like object.
    * @tparam Y the underlying type of <code>ys</code> and the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def build[Y](ys: Seq[Y]): ListLike[Y] = ys match {
    case h :: t => MyLazyList(h, t)
    case _ => MyLazyList(ys)
  }

  override def iterator: Iterator[Nothing] = Iterator.empty
}

abstract class LazyListLike[+X] extends ListLike[X] {

  /**
    * The "map" function.
    * NOTE: that we have defined the <code>map</code> function in terms of <code>flatMap</code> and the
    * <code>apply</code> function (sometimes known as the "unit" function).
    *
    * @param f a function which converts an <code>X<code into a <code>Y<code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>LazyList[Y]<code> where each element is the result of applying <code>f<code to the corresponding
    *         element of <code>this<code>.
    */
  def map[Y](f: X => Y): ListLike[Y] = flatMap(x => unit[Y](f(x)))

  /**
    * Construct a (finite) <code>LazyList[X]</code> with exactly one element.
    *
    * @param y the value of the element.
    * @tparam Y the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def unit[Y](y: Y): ListLike[Y] = MyLazyList(y)

  /**
    * ToString method.
    *
    * @return a String which shows the head but leaves the tail as question marks.
    */
  override def toString = s"$head, ???"

}

object FunctionalUtility {
  def invert[X](p: X => Boolean): X => Boolean = !p(_)
}

object MyLazyList {

  /**
    * Construct a (finite) <code>LazyList[X]</code> with exactly one element.
    *
    * @param x the value of the element.
    * @tparam X the underlying type of the result.
    * @return a <code>ListLike[X]</code> with exactly one element (whose value is <code>x</code>).
    */
  def apply[X](x: X): ListLike[X] = MyLazyList(x, () => EmptyList)

  /**
    * Construct a (finite) <code>ListLike[X]</code> corresponding to a sequence.
    *
    * @param xs the sequence.
    * @tparam X the underlying type of the result.
    * @return a <code>ListLike[X]</code> with the same number of elements as <code>xs</code>.
    */
  def apply[X](xs: Seq[X]): ListLike[X] = xs match {
    case Nil => EmptyList
    case h :: t => MyLazyList(h, () => apply(t))
  }

  /**
    * Construct a LazyList from a head element and a tail of a Stream
    *
    * @param x  the head of the new list-like object
    * @param xs the sequence.
    * @tparam X the underlying type of the result.
    * @return a <code>ListLike[X]</code> with the same number of elements as <code>xs</code>.
    */
  def apply[X](x: X, xs: Seq[X]): ListLike[X] = MyLazyList(x, () => apply(xs))

  /**
    * Construct a stream of xs.
    *
    * CONSIDER should we use call-by-value here instead of call-by-name?
    *
    * @param x the call-by-name value to be repeated
    * @tparam X the type of X
    * @return a <code>ListLike[X]</code> with an infinite number of element (whose values are <code>x</code>).
    */
  def continually[X](x: => X): ListLike[X] = MyLazyList(x, () => continually(x))

  /**
    * A lazy val definition of a stream of 1s.
    */
  lazy val ones: ListLike[Int] = MyLazyList(1, () => ones)

  /**
    * Method to yield a singleton LazyList
    *
    * @param x the single value.
    * @tparam X the underlying type of x.
    * @return a MyLazyList with just x as a value.
    */
  def singleton[X](x: => X): ListLike[X] = MyLazyList(x, () => EmptyList)

  /**
    * Construct a stream of Integers starting with <code>start</code> and with successive elements being
    * greater than their predecessors by <code>step</code>.
    *
    * @param start the value of the first element.
    * @param step  the difference between successive elements.
    * @return a <code>ListLike[X]</code> with an infinite number of element (whose values are <code>x</code>,
    *         <code>x+step</code>, etc.).
    */
  def from(start: Int, step: Int): ListLike[Int] = ??? // TO BE IMPLEMENTED

  /**
    * Construct a stream of Integers starting with <code>start</code> and with successive elements being
    * the next greater Int.
    *
    * @param start the value of the first element.
    * @return a <code>ListLike[X]</code> with an infinite number of element (whose values are <code>x</code>,
    *         <code>x+1</code>, etc.).
    */
  def from(start: Int): ListLike[Int] = from(start, 1)
}

case class LazyListException(w: String) extends Exception(s"LazyList exception: $w")
