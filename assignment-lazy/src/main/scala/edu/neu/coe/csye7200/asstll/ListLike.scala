/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.asstll

/**
  * Trait to define the behavior of something that behaves like a list.
  *
  * This trait is extended by the LazyList classes.
  *
  * @tparam X the underlying type of this Monad
  */
trait ListLike[+X] extends Monadic[X] {
  self =>

  /**
    * Method to yield the size of this list-like object.
    *
    * @return Some(n) where n is size if it's definite; if size is not known (lazy) then return None
    */
  def size(): Option[Int]

  /**
    * Construct a (finite) <code>ListLike[X]</code> with exactly one element.
    * Overrides the definition from Monadic by making the result type more specific.
    *
    * Note that this is an instance method.
    *
    * @param y the value of the element.
    * @tparam Y the type of <code>y</code> and the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def unit[Y](y: Y): ListLike[Y]

  /**
    * Construct a <code>ListLike[X]</code> with the elements of <code>ys</code>.
    *
    * Note that this is an instance method.
    *
    * @param ys the sequence of elements with which to construct the list-like object.
    * @tparam Y the underlying type of <code>ys</code> and the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def build[Y](ys: Seq[Y]): ListLike[Y]

  /**
    * The "map" function.
    * Overrides the definition from Monadic by making the result type more specific.
    *
    * @param f a function which converts an <code>X</code> into a <code>Y</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>ListLike[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code>.
    */
  def map[Y](f: X => Y): ListLike[Y]

  /**
    * The "flatMap" function.
    * Overrides the definition from Monadic by making the result type more specific.
    *
    * @param f a function which converts an <code>X</code> into a <code>LazyList[Y]</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>ListLike[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code> and then "flattening" the result
    *         by concatenating all streams together.
    */
  def flatMap[Y](f: X => Monadic[Y]): ListLike[Y]

  /**
    * The "filter" function.
    * Overrides the definition from Monadic by making the result type more specific.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filter(p: X => Boolean): ListLike[X]

  /**
    * The "filterNot" function.
    * Overrides the definition from Monadic by making the result type more specific.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filterNot(p: X => Boolean): ListLike[X]

  /**
    * Method to determine if this ListLike object is empty.
    *
    * @return true if the list is empty.
    */
  def isEmpty: Boolean

  /**
    * @return the head of this list
    */
  def head: X

  /**
    * @return the tail of this list
    */
  def tail: ListLike[X]

  /**
    * Method to form a new list-like object by pre-pending y
    *
    * @param y the value to serve as the new head
    * @tparam Y the type of y
    * @return a new <code>ListLike[Y]</code> object with y as its head and this as its tail
    */
  def +:[Y >: X](y: Y): ListLike[Y]

  /**
    * Concatenate this LazyList with ys.
    *
    * CONSIDER moving to LazyListLike
    *
    * @param ys the stream to be used if/when this stream is exhausted.
    * @tparam Y the underlying type of ys and the result.
    * @return a <code>ListLike[Y]</code> which contains all the elements of this followed by all the elements of ys.
    */
  def ++[Y >: X](ys: ListLike[Y]): ListLike[Y]

  /**
    * Method to "zip" to ListLike objects together
    *
    * @param ys the stream of Ys
    * @tparam Y the underlying type of <code>ys</code>
    * @return a <code>ListLike[(X,Y)]</code> where each element is a tuple of the corresponding elements from this
    *         and ys respectively.
    */
  def zip[Y](ys: ListLike[Y]): ListLike[(X, Y)]

  /**
    * Take the first n elements of this ListLike object as a ListLike object.
    *
    * @param n the number of elements to take (must not be negative).
    * @return a <code>ListLike[(X,Y)]</code> of length n.
    */
  def take(n: Int): ListLike[X]

  /**
    * Drop the first n elements of this ListLike object and return the remainder as a ListLike object.
    *
    * @param n the number of elements to drop (must not be negative).
    * @return a <code>ListLike[(X,Y)]</code> which is shorter than this by n.
    */
  def drop(n: Int): ListLike[X]

  /** Necessary to keep this from being implicitly converted to
    * <code>[[scala.collection.Iterable]]</code> in `for` comprehensions.
    */
  @inline final def withFilter(p: X => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
    * collection" contract.
    */
  class WithFilter(p: X => Boolean) {
    def map[B](f: X => B): ListLike[B] = self filter p map f

    def flatMap[B](f: X => ListLike[B]): ListLike[B] = self filter p flatMap f

    //    def foreach[U](f: X => U): Unit = self filter p foreach f
    def withFilter(q: X => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /**
    * Method to flatten a <code>ListLike[Monadic[Y]</code>
    * This is an instance method because it used the build method.
    * It does NOT refer to any other information about this ListLike object.
    *
    * @param ms a <code>ListLike[Seq[Y]</code> to be flattened
    * @tparam Y the underlying type of ms and the result
    * @return a <code>ListLike[(X,Y)]</code>
    */
  def flatten[Y](ms: ListLike[Seq[Y]]): ListLike[Y] = flatten(ms.toSeq)

  /**
    * Method to flatten a <code>ListLike[Monadic[Y]</code>
    * This is an instance method because it used the build method.
    * It does NOT refer to any other information about this ListLike object.
    *
    * CONSIDER simplifying
    *
    * @param yss <code>Seq[Seq[Y]</code> to be flattened
    * @tparam Y the underlying type of ms and the result
    * @return a <code>ListLike[(X,Y)]</code>
    */
  def flatten[Y](yss: Seq[Seq[Y]]): ListLike[Y] = build(yss.foldLeft[Seq[Y]](Seq())((_: Seq[Y]) ++ (_: Seq[Y])))
}

/**
  * Trait to define functor behavior
  *
  * @tparam X the underlying type of this Functor
  */
trait Functor[+X] {

  /**
    * The "map" function.
    *
    * @param f a function which converts an <code>X</code> into a <code>Y</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>Functor[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code>.
    */
  def map[Y](f: X => Y): Functor[Y]

}

/**
  * Trait to define monadic behavior.
  *
  * @tparam X the underlying type of this Monad
  */
trait Monadic[+X] extends Functor[X] {
  /**
    * Construct a (finite) <code>LazyList[X]</code> with exactly one element.
    *
    * @param y the value of the element.
    * @tparam Y the underlying type of the resulting monad
    * @return a <code>Monadic[Y]</code> with exactly one element (whose value is <code>x</code>).
    */
  def unit[Y >: X](y: Y): Monadic[Y]

  /**
    * The "map" function.
    * Overrides the definition form Functor by making the result type more specific.
    *
    * @param f a function which converts an <code>X</code> into a <code>Y</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>Monadic[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code>.
    */
  def map[Y](f: X => Y): Monadic[Y]

  /**
    * The "flatMap" function.
    *
    * @param f a function which converts an <code>X</code into a <code>LazyList[Y]</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>LazyList[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code> and then "flattening" the result
    *         by concatenating all streams together.
    */
  def flatMap[Y](f: X => Monadic[Y]): Monadic[Y]

  /**
    * The "filter" function.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>Monadic[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filter(p: X => Boolean): Monadic[X]

  /**
    * The "filterNot" function.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filterNot(p: X => Boolean): Monadic[X]

  /**
    * Convert this <code>Monadic[X]</code> into a <code>Seq[X]</code>, element for element.
    *
    * @return a <code>Seq[X]</code>
    */
  def toSeq: Seq[X]
}
