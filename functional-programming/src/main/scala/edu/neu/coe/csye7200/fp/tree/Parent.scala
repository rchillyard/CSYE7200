package edu.neu.coe.csye7200.fp.tree

import scala.annotation.tailrec

/**
  * A trait which expresses parenthood.
  * This defines a type class.
  *
  * @tparam T the underlying type of the Parent
  */
trait Parent[T] {
  /**
    * Get the children of a T.
    *
    * @param t the parent whose children we want
    * @return the children as a Seq of T
    */
  def children(t: T): Seq[T]
}

object Parent {
  /**
    * Tail-recursive method to traverse a tree-like object (defined as something that implements Parent).
    *
    * @param f  the map function which takes a T and produces an R for just that parent object (not its children).
    * @param g  the reduce function which accumulates the result (the accumulator is passed as its first parameter).
    * @param q  the quick return function which, if q(r) yields true, the method immediately returns r.
    * @param z  the function which builds the T list from the existing T list and the given T (parent node).
    * @param ts a list of Ts to be worked on.
    * @param r  the current value of the result, i.e. the "accumulator".
    * @tparam T a type which extends Parent, and thus has children of type T -- this "context bound" is implemented via a compiler-generated implicit parameter of type Parent[T].
    * @tparam R the result type.
    * @return a value of R.
    */
  @tailrec
  final def traverse[T: Parent, R](f: T => R, g: (R, R) => R, q: R => Boolean, z: (List[T], T) => List[T])(ts: List[T], r: R): R =
    if (q(r))
      r
    else
      ts match {
        case Nil => r
        case h :: t => traverse(f, g, q, z)(z(t, h), g(r, f(h)))
      }
}
