/*
 * Copyright (c) 2018. Phasmid Software
 */

package edu.neu.coe.csye7200.asstll

case class MappedList[W, +X](ws: Seq[W], f: W => X) extends ListLike[X] {

  /**
    * Convert this <code>ListLike[X]</code> into a <code>Seq[X]</code>, element for element.
    *
    * @return a <code>Seq[X]</code>
    */
  def toSeq: Seq[X] = ws map f

  /**
    * Method to yield the size of this list-like object.
    *
    * @return Some(n) where n is size if it's definite; if size is not known (lazy) then return None
    */
  def size(): Option[Int] = Some(ws.size)

  /**
    * Construct a (finite) <code>ListLike[X]</code> with exactly one element.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * Note that this is an instance method.
    *
    * @param y the value of the element.
    * @tparam Y the type of <code>y</code> and the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def unit[Y](y: Y): ListLike[Y] = build(Seq(y))

  /**
    * The "map" function.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * @param g a function which converts an <code>X</code> into a <code>Y</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>ListLike[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code>.
    */
  def map[Y](g: X => Y): ListLike[Y] = MappedList[W, Y](ws, f andThen g)

  /**
    * The "flatMap" function.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * @param g a function which converts an <code>X</code> into a <code>LazyList[Y]</code>.
    * @tparam Y the underlying type of the result.
    * @return a <code>ListLike[Y]</code> where each element is the result of applying <code>f</code> to the corresponding
    *         element of <code>this</code> and then "flattening" the result
    *         by concatenating all streams together.
    */
  def flatMap[Y](g: X => Monadic[Y]): ListLike[Y] = {
    val z: Seq[Seq[Y]] = ws map (f andThen g andThen { x => x.toSeq })
    flatten[Y](z)
  }

  /**
    * The "filter" function.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filter(p: X => Boolean): ListLike[X] = build(toSeq).filter(p)

  /**
    * The "filterNot" function.
    * Overrides the definition form Monadic by making the result type more specific.
    *
    * @param p a predicate which takes an <code>X</code> and yields a <code>Boolean</code>.
    * @return a <code>ListLike[X]</code> where every element satisfies the predicate <code>p</code>.
    */
  def filterNot(p: X => Boolean): ListLike[X] = filter(FunctionalUtility.invert(p))

  /**
    * Method to determine if this ListLike object is empty.
    *
    * @return true if the list is empty.
    */
  def isEmpty: Boolean = (for (x <- size()) yield x == 0).get

  /**
    * @return the head of this list
    */
  def head: X = f(ws.head)

  /**
    * @return the tail of this list
    */
  def tail: ListLike[X] = ws match {
    case _ :: t => MappedList(t, f)
    case Nil => MappedList.empty
  }

  /**
    * Method to form a new list-like object by pre-pending y
    *
    * @param y the value to serve as the new head
    * @tparam Y the type of y
    * @return a new <code>ListLike[Y]</code> object with y as its head and this as its tail
    */
  def +:[Y >: X](y: Y): ListLike[Y] = build[Y](y +: ws.asInstanceOf[Seq[Y]])

  /**
    * Concatenate this LazyList with ys.
    *
    * CONSIDER moving to LazyListLike
    *
    * @param ys the stream to be used if/when this stream is exhausted.
    * @tparam Y the underlying type of ys and the result.
    * @return a <code>ListLike[Y]</code> which contains all the elements of this followed by all the elements of ys.
    */
  def ++[Y >: X](ys: ListLike[Y]): ListLike[Y] = build(toSeq ++ ys.toSeq)

  /**
    * Method to "zip" to ListLike objects together
    *
    * @param ys the stream of Ys
    * @tparam Y the underlying type of <code>ys</code>
    * @return a <code>ListLike[(X,Y)]</code> where each element is a tuple of the corresponding elements from this
    *         and ys respectively.
    */
  def zip[Y](ys: ListLike[Y]): ListLike[(X, Y)] = build(toSeq.zip(ys.toSeq))

  /**
    * Take the first n elements of this ListLike object as a ListLike object.
    *
    * @param n the number of elements to take (must not be negative).
    * @return a <code>ListLike[(X,Y)]</code> of length n.
    */
  def take(n: Int): ListLike[X] = MappedList(ws take n, f)

  /**
    * Drop the first n elements of this ListLike object and return the remainder as a ListLike object.
    *
    * @param n the number of elements to drop (must not be negative).
    * @return a <code>ListLike[(X,Y)]</code> which is shorter than this by n.
    */
  def drop(n: Int): ListLike[X] = MappedList(ws drop n, f)

  /**
    * Construct a <code>ListLike[X]</code> with the elements of <code>ys</code>.
    *
    * Note that this is an instance method.
    *
    * @param ys the sequence of elements with which to construct the list-like object.
    * @tparam Y the underlying type of <code>ys</code> and the underlying type of the resulting ListLike object
    * @return a <code>ListLike[Y]</code> with exactly one element (whose value is <code>y</code>).
    */
  def build[Y](ys: Seq[Y]): ListLike[Y] = MappedList.create(ys)

  override def hashCode(): Int = ws match {
    case Nil => 0
    case _ => ws.hashCode() + 31 * f.hashCode()
  }

  /**
    * Equals method.
    * NOTE: I'm not sure why we have to provide explicit return statements.
    * But we do.
    *
    * @param obj the other object to be compared with this.
    * @return true if this is equal to obj.
    */
  override def equals(obj: scala.Any): Boolean = {
    val identical = obj match {
      case r: AnyRef => r eq this
      case _ => false
    }
    if (identical) return true
    if (getClass == obj.getClass) {
      val other = obj.asInstanceOf[MappedList[W, X]]
      ws match {
        case Nil =>
          return other.ws == Nil
        case _ =>
          return ws == other.ws && f == other.f
      }
    }
    false
  }

}

object MappedList {

  def create[X](xs: Seq[X]): MappedList[X, X] = new MappedList(xs, identity)

  def apply[X](x: X): MappedList[X, X] = create(Seq(x))

  def apply[X](xs: X*): MappedList[X, X] = create(xs)

  val empty: MappedList[Nothing, Nothing] = MappedList[Nothing, Nothing](Nil, identity)

}
