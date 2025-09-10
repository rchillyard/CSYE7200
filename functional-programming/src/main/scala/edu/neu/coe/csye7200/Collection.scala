package edu.neu.coe.csye7200

import scala.concurrent.{ExecutionContext, Future}

/**
 * A trait representing a collection that provides various operations such as mapping, aggregation,
 * reduction, and conversion to a list. This trait extends `IterableOnce`, allowing it to be iterated over
 * once to traverse its elements.
 *
 * XXX 1 (Collection)
 *
 * It is expected that evaluating the collection will take significant resources and therefore should be
 * implemented asynchronously.
 * Thus, the methods `sizeAsync`, `listAsync`, `aggregateAsync`, `reduceAsync` are implemented asynchronously.
 *
 * @tparam T the type of elements contained in the collection
 */
trait Collection[T] extends IterableOnce[T] {
  /**
   * Transformation that lazily applies the provided function `g` to this collection
   * and returns a new collection based on `g` and `this` collection.
   * It is to be expected that implementations of this method are lazy such that the actual invocation of `g`
   * s deferred until an action is taken.
   *
   * @param g a function that takes an element of type `T` and transforms it to an element of type `U`.
   * @return a new collection containing the elements resulting from applying `g` to each element of this collection.
   */
  def map[U](g: T => U): Collection[U]

  /**
   * Action that asynchronously evaluates the size of the collection.
   *
   * @param ec an implicit `ExecutionContext` which provides the execution context
   *           for the asynchronous computation.
   * @return a `Future` containing the size of the collection as an `Int`.
   */
  def sizeAsync(implicit ec: ExecutionContext): Future[Int]

  /**
   * Action that asynchronously converts the collection into a `Future` containing a `List` of its elements.
   *
   * @param ec an implicit `ExecutionContext` required to execute the asynchronous operation.
   * @return a `Future` containing a `List` of elements of type `T`.
   */
  def listAsync(implicit ec: ExecutionContext): Future[List[T]]

  /**
   * Action that asynchronously aggregates the elements of the collection into a single result using the provided
   * initial value and aggregation function.
   *
   * The aggregation process starts with the lazy initialization of the provided value `u` and
   * then sequentially applies the aggregation function `g` on each element of the collection.
   * Thus, the behavior of `aggregateAsync` is essentially similar to `foldLeft`.
   *
   * The result of the aggregation is computed asynchronously, with the help of the execution context.
   *
   * @param u  the initial value for the aggregation, evaluated lazily
   * @param g  a function that takes an accumulated value of type `U` and an element of type `T`, producing a new accumulated value of type `U`
   * @param ec the execution context used to execute the aggregation asynchronously; must be provided implicitly
   * @return a `Future` holding the resulting aggregated value of type `U` once the computation completes
   */
  def aggregateAsync[U](u: => U)(g: (U, T) => U)(implicit ec: ExecutionContext): Future[U]

  /**
   * Action to perform an asynchronous reduction operation on the elements of the collection.
   * The reduction function combines two elements of type `T` and returns a single result of type `T`.
   * The operation is performed asynchronously in a provided execution context.
   *
   * @param g  a binary function `(T, T) => T` used to reduce the elements of the collection
   * @param ec an implicit `ExecutionContext` in which the asynchronous computation will be executed
   * @return a `Future[T]` containing the result of the reduction if the collection has at least one element;
   *         the future will fail if the collection is empty
   */
  def reduceAsync(g: (T, T) => T)(implicit ec: ExecutionContext): Future[T]
}

/**
 * Provides functionality to wrap iterators or iterables into lazily evaluated `Collection` instances.
 *
 * The `Collection` object acts as a companion to the `Collection` trait, offering utility methods
 * to create concrete implementations of `Collection` based on `Iterator` or `Iterable` inputs. The
 * lazy nature of these collections allows for deferred evaluation while supporting various functional
 * operations.
 */
object Collection {
  /**
   * Wraps the provided iterator into a `Collection`.
   *
   * This function creates a concrete lazy collection based on the provided iterator.
   * The resulting collection wraps the elements from the iterator for further processing
   * and support for collection operations provided by the `Collection` trait.
   *
   * @param iterator an `Iterator` containing the elements to be wrapped in the `Collection`.
   * @tparam T the type of elements contained within the iterator and resulting collection.
   * @return a `Collection` wrapping the elements of the provided iterator.
   */
  def apply[T](iterator: Iterator[T]): Collection[T] = LazyCollectionOnceOnly[T, T](iterator)(identity)

  /**
   * Wraps the elements of the given `Iterable` into a `Collection`.
   *
   * This method creates a concrete lazy collection that processes the elements from the provided
   * `Iterable`. It internally converts the `Iterable` into an `Iterator` and delegates to the
   * overloaded `apply` method that takes an `Iterator`.
   *
   * @param iterable an `Iterable` containing the elements to be encapsulated into the `Collection`.
   * @tparam T the type of the elements contained in the provided `Iterable`.
   * @return a `Collection` wrapping the elements of the provided `Iterable`.
   */
  def apply[T](iterable: Iterable[T]): Collection[T] = apply(iterable.iterator)
}

/**
 * An abstract base class for lazily evaluated collections, where data transformation is applied
 * to elements of an underlying iterator, and provides both synchronous and asynchronous operations.
 *
 * This class leverages laziness of computation to ensure efficient transformation of data during
 * synchronous operations like mapping and iteration. At the same time, it supports asynchronous
 * execution for expensive operations like size calculation, listing, aggregation, and reduction.
 *
 * The class is parameterized by two types:
 * - `S`: the type of elements in the source iterator.
 * - `T`: the type of elements in the transformed (current) collection.
 *
 * This class extends the `Collection` trait, inheriting its methods and enabling transformation
 * and computation over collections in both synchronous and asynchronous contexts.
 * XXX 2 (abstract class LazyCollectionBase[S, T]...)
 *
 * @constructor Creates a LazyCollectionBase with an underlying iterator of type `S` and a transformation function `f`.
 * @param xs the underlying iterator providing lazy sequential elements of type `S` (and private to the csye7200 package).
 *           XXX 3a (Iterator[S])
 *           XXX 3b (private[csye7200] val...)
 * @param f  the transformation function that maps elements of type `S` to elements of type `T`.
 * @tparam S the original type of the underlying elements.
 * @tparam T the effective type of this collection once it has been fully evaluated by an action.
 */
abstract class LazyCollectionBase[S, T](private[csye7200] val xs: Iterator[S], f: S => T) extends Collection[T] {

  // Synchronous methods (these are fast because they are implemented lazily) ...

  /**
   * Returns an iterator over the elements of this collection after applying the function `f` to each element.
   *
   * @return an Iterator of type T containing the transformed elements of the collection.
   */
  // TODO (4) you have available `xs` and `f` to work with.
  // This is actually easier than you might think.
  // Take care that you don't just return `xs`.
  def iterator: Iterator[T] = xs.map(f)

  /**
   * Abstract method to create a new `Collection[U]` based on the given collection and the function `g`.
   *           XXX 4a (unit)
   *           XXX 4b (unit)
   *
   * @param collection the input collection containing elements of type `T`
   * @param g          a function that transforms an object from type `S` to type `U`
   * @tparam U the type of the resulting collection
   * @return a new Collection[U]
   */
  def unit[U](collection: Collection[T])(g: S => U): Collection[U]

  /**
   * Applies the given function `g` to each element of this collection
   * and produces a new collection containing the transformed elements.
   *
   * @param g a function that takes an element of type `T` and transforms it into an element of type `U`.
   * @return a `Collection[U]` containing the elements resulting from applying `g` to each element of this collection.
   */
  // TODO (8) you have an easy way to do this: the `unit` method. You just have to determine the required function.
  def map[U](g: T => U): Collection[U] = unit[U](this)(f andThen g)

  // Asynchronous methods...

  /**
   * Computes the size of the collection asynchronously.
   *
   * @param ec The execution context in which the asynchronous computation is performed.
   * @return A future containing the size of the collection represented as an integer.
   */
  def sizeAsync(implicit ec: ExecutionContext): Future[Int] = Future(iterator.size)

  /**
   * Asynchronously converts the elements of this collection into a `List`.
   * This method works by collecting all elements from the underlying iterator of the collection.
   *
   * @param ec an implicit `ExecutionContext` to handle the asynchronous computation.
   * @return a `Future` containing a `List` of elements of type `T` from this collection.
   */
  def listAsync(implicit ec: ExecutionContext): Future[List[T]] = Future(iterator.toList)

  /**
   * Aggregates the elements of the collection asynchronously by applying a given function `g`,
   * starting with an initial value `u`.
   * CONSIDER renaming this as foldLeftAsync since that's basically what it is.
   *
   * @param u  the initial value of type `U` to be combined with the elements of the collection.
   * @param g  a bi-function that combines the current aggregated value of type `U` with
   *           each element of type `T` in the collection, producing a new aggregated value of type `U`.
   * @param ec an implicit `ExecutionContext` used to execute the asynchronous computation.
   * @return a `Future[U]` representing the result of the aggregation operation.
   */
  // TODO (6) you'll need to use foldLeft, u, g, and iterator (like the methods above and `reduce` below).
  def aggregateAsync[U](u: => U)(g: (U, T) => U)(implicit ec: ExecutionContext): Future[U] = Future(iterator.foldLeft(u)(g))

  /**
   * Asynchronously reduces the elements of this collection using the provided associative binary function `g`.
   *
   * @param g  A binary function that combines two elements of type `T` into one.
   * @param ec An implicit `ExecutionContext` used for asynchronous computation.
   * @return A `Future` containing the result of the reduction,
   *         or a `Future` with a `NoSuchElementException` if the collection is empty.
   */
  def reduceAsync(g: (T, T) => T)(implicit ec: ExecutionContext): Future[T] = Future(iterator.reduce(g))
}

/**
 * Represents a lazily evaluated collection that applies a transformation function `f`
 * to elements from an underlying iterator exactly once.
 *
 * This class is a concrete implementation of `LazyCollectionBase`, providing the ability
 * to work with collections whose elements are consumed only once, ensuring the integrity
 * of the lazy evaluation process. It supports efficient transformation and computation
 * over the collection without immediately triggering evaluation.
 *
 * @constructor Creates a `LazyCollectionOnceOnly` from an underlying iterator of type `S` and
 *              a transformation function `f` that maps elements of type `S` to elements of type `T`.
 * @param ts the source iterator providing elements of type `S`.
 * @param f  the transformation function of type `S => T` that is applied to each element of the iterator.
 * @tparam S the type of elements in the underlying source iterator.
 * @tparam T the type of transformed elements in the resulting collection.
 */
case class LazyCollectionOnceOnly[S, T](private val ts: Iterator[S])(f: S => T) extends LazyCollectionBase[S, T](ts, f) {

  /**
   * Creates a new `LazyCollectionOnceOnly[U]` based on the given collection and the function `g`.
   *
   * @param collection the input collection containing elements of type `T`
   * @param g          a function that transforms an object from type `S` to type `U`
   * @return a new LazyCollectionOnceOnly[U]
   */
  def unit[U](collection: Collection[T])(g: S => U): Collection[U] = collection match {
    // TODO (10) Basically, you need to return a concrete class which implements Collection[U].
    // Be sure to ignore any instance fields from `this`.
    // You should only use the types S and T from `this` together with the parameters provided to `unit`.
    // XXX 5 (c)
    case c: LazyCollectionBase[S, T] @unchecked => LazyCollectionOnceOnly(c.xs)(g)
  }

}