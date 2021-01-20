//package edu.neu.coe.csye7200

///**
//  * Created by scalaprof on 8/1/16.
//  */
//case class Container[+A](as: Seq[A]) extends FilterMonadic[A,Container[A]] with TraversableLike[A,Container[A]] { self =>
//
//  override def map[B, That](f: (A) => B)(implicit bf: CanBuildFrom[Container[A], B, That]): That = ???
//
//  override def flatMap[B, That](f: (A) => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Container[A], B, That]): That = ???
//
//  override def withFilter(p: (A) => Boolean): FilterMonadic[A, Container[A]] = new WithFilter(p1)
//
//  override def foreach[U](f: (A) => U): Unit = as foreach(f)
//
//  /** The collection of type $coll underlying this `TraversableLike` object.
//    *  By default this is implemented as the `TraversableLike` object itself,
//    *  but this can be overridden.
//    */
//  override def repr: Container[A] = this.asInstanceOf[Container[A]]
//
//  /** A class supporting filtered operations. Instances of this class are
//    *  returned by method `withFilter`.
//    */
//  class WithFilter(p: A => Boolean) extends FilterMonadic[A, Container[A]] {
//
//    /** Builds a new collection by applying a function to all elements of the
//      *  outer $coll containing this `WithFilter` instance that satisfy predicate `p`.
//      *
//      *  @param f      the function to apply to each element.
//      *  @tparam B     the element type of the returned collection.
//      *  @tparam That  $thatinfo
//      *  @param bf     $bfinfo
//      *  @return       a new collection of type `That` resulting from applying
//      *                the given function `f` to each element of the outer $coll
//      *                that satisfies predicate `p` and collecting the results.
//      *  @usecase def map[B](f: A => B): $Coll[B]
//      *    @inheritdoc
//      *    @return       a new $coll resulting from applying the given function
//      *                  `f` to each element of the outer $coll that satisfies
//      *                  predicate `p` and collecting the results.
//      */
//    def map[B, That](f: A => B)(implicit bf: CanBuildFrom[Container[A], B, That]): That = {
//      val b = bf(repr)
//      for (x <- self)
//        if (p(x)) b += f(p1)
//      b.result
//    }
//
//    /** Builds a new collection by applying a function to all elements of the
//      *  outer $coll containing this `WithFilter` instance that satisfy
//      *  predicate `p` and concatenating the results.
//      *
//      *  @param f      the function to apply to each element.
//      *  @tparam B     the element type of the returned collection.
//      *  @tparam That  $thatinfo
//      *  @param bf     $bfinfo
//      *  @return       a new collection of type `That` resulting from applying
//      *                the given collection-valued function `f` to each element
//      *                of the outer $coll that satisfies predicate `p` and
//      *                concatenating the results.
//      *  @usecase def flatMap[B](f: A => TraversableOnce[B]): $Coll[B]
//      *    @inheritdoc
//      *
//      *    The type of the resulting collection will be guided by the static type
//      *    of the outer $coll.
//      *    @return       a new $coll resulting from applying the given
//      *                  collection-valued function `f` to each element of the
//      *                  outer $coll that satisfies predicate `p` and concatenating
//      *                  the results.
//      */
//    def flatMap[B, That](f: A => GenTraversableOnce[B])(implicit bf: CanBuildFrom[Container[A], B, That]): That = {
//      val b = bf(repr)
//      for (x <- self)
//        if (p(x)) b ++= f(p1).seq
//      b.result
//    }
//
//    /** Applies a function `f` to all elements of the outer $coll containing
//      *  this `WithFilter` instance that satisfy predicate `p`.
//      *
//      *  @param  f   the function that is applied for its side-effect to every element.
//      *              The result of function `f` is discarded.
//      *  @tparam  U  the type parameter describing the result of function `f`.
//      *              This result will always be ignored. Typically `U` is `Unit`,
//      *              but this is not necessary.
//      *  @usecase def foreach(f: A => Unit): Unit
//      *    @inheritdoc
//      */
//    def foreach[U](f: A => U): Unit =
//      for (x <- self)
//        if (p(x)) f(p1)
//
//    /** Further refines the filter for this $coll.
//      *
//      *  @param q   the predicate used to test elements.
//      *  @return    an object of class `WithFilter`, which supports
//      *             `map`, `flatMap`, `foreach`, and `withFilter` operations.
//      *             All these operations apply to those elements of this $coll which
//      *             satisfy the predicate `q` in addition to the predicate `p`.
//      */
//    def withFilter(q: A => Boolean): WithFilter =
//      new WithFilter(x => p(p1) && q(p1))
//  }
//
//  override protected[this] def newBuilder: mutable.Builder[A, Container[A]] = new ListBuffer[A]()
//
//  override def seq: TraversableOnce[A] = as
//}
