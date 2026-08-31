package edu.neu.coe.csye7200.lab99.container

/**
 * A generic trait representing a container that holds a value of type `T`. It provides methods
 * to access, transform, and filter the contained value, as well as perform operations that 
 * require side effects.
 *
 * @tparam T The type of the value held by the container. This type is covariant.
 */
trait Container[+T]:
  /**
   * Checks if the container is empty.
   *
   * @return true if the container is empty, false otherwise.
   */
  def isEmpty: Boolean

  /**
   * Retrieves the value contained within the instance.
   *
   * @return The value of type `T` held by the container.
   */
  def get: T

  /**
   * Transforms the value contained in the container using the provided function `f`
   * and returns a new container holding the result.
   *
   * @param f A function that maps the contained value of type `T` to a new value of type `U`.
   * @return A new container of type `Container[U]` holding the transformed value.
   */
  def map[U](f: T => U): Container[U]

  /**
   * Transforms the value contained in the container using the provided function `f`, 
   * which maps the contained value to another container. The returned container will 
   * hold the resulting transformed value, effectively "flattening" nested containers.
   *
   * @param f A function that takes the contained value of type `T` and returns a new 
   *          container of type `Container[U]`.
   *
   * @return A single container of type `Container[U]` resulting from applying the function `f`
   *         and flattening the nested structure.
   */
  def flatMap[U](f: T => Container[U]): Container[U]

  /**
   * Filters the value contained in the container using the provided predicate function `p`.
   * If the value satisfies the predicate, it remains in the container; otherwise, 
   * an appropriate empty container is returned.
   *
   * @param p A predicate function that takes the contained value of type `T`
   *          and returns `true` to keep the value or `false` to remove it.
   *
   * @return A container of type `Container[T]` that holds the filtered value.
   */
  def filter(p: T => Boolean): Container[T]

  /**
   * Applies the provided function `f` to the value contained in the container.
   * This method is typically used for side-effecting operations.
   *
   * @param f A function that takes the contained value of type `T` and performs a side-effecting operation.
   * @return Unit, as this method is used for side effects and does not produce a result.
   */
  def foreach(f: T => Unit): Unit

case class MyContainer[T](t: T) extends Container[T]:
  def isEmpty: Boolean =
    // TO BE IMPLEMENTED 
    ???

  def get: T =
    // TO BE IMPLEMENTED 
    ???

  def map[U](f: T => U): Container[U] =
    // TO BE IMPLEMENTED 
    ???

  def flatMap[U](f: T => Container[U]): Container[U] =
    // TO BE IMPLEMENTED 
    ???

  def filter(p: T => Boolean): Container[T] =
    // TO BE IMPLEMENTED 
    ???

  def foreach(f: T => Unit): Unit = {
    // TO BE IMPLEMENTED 
        ???
  }

case object EmptyContainer extends Container[Nothing]:
  def isEmpty: Boolean =
    // TO BE IMPLEMENTED 
    ???

  def get: Nothing =
    // TO BE IMPLEMENTED 
    ???

  def map[U](f: Nothing => U): Container[U] =
    // TO BE IMPLEMENTED 
    ???

  def flatMap[U](f: Nothing => Container[U]): Container[U] =
    // TO BE IMPLEMENTED 
    ???

  def filter(p: Nothing => Boolean): Container[Nothing] =
  // TO BE IMPLEMENTED 
    ???

  def foreach(f: Nothing => Unit): Unit = {
  // TO BE IMPLEMENTED 
    ???
  }
     