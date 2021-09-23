package edu.neu.coe.csye7200.fp

import scala.util.{Success, Try}


trait Monad[M[_]] extends Functor[M] {
  /**
    * "unit" method which will take an A and return an M[A]
    *
    * @param a the value of a to be wrapped in M
    * @tparam A the type of a
    * @return an instance of M[A] based on a
    */
  def unit[A](a: A): M[A]

  /**
    * "bind" method which will take an M[A] and a function A => M[B] and yields a M[B].
    * In Scala parlance, this is known as flatMap.
    *
    * @param am an instance of M[A]
    * @param f  a function A => M[B]
    * @tparam A the type of A
    * @tparam B the underlying type of the result
    * @return an instance of M[B] based on am and f.
    */
  def bind[A, B](am: M[A])(f: A => M[B]): M[B]

  /**
    * The "map" method which takes an M[A] and a function A => B and yields a M[B].
    *
    * @param am an instance of M[A]
    * @param f  a function A => B
    * @tparam A the underlying type of M[A]
    * @tparam B the underlying type of the result
    * @return an instance of M[B] based on am and f.
    */
  def map[A, B](am: M[A])(f: A => B): M[B] = bind(am)(a => unit[B](f(a)))

  /**
    * The "map" method which takes an M[A], an M[B] and a function (A,B) => C and yields a M[C].
    * Also known as "map2"
    *
    * @param am an instance of M[A]
    * @param bm an instance of M[B]
    * @param f  a function (A,B) => C
    * @tparam A the underlying type of M[A]
    * @tparam B the underlying type of M[B]
    * @tparam C the underlying type of the result
    * @return an instance of M[C] based on am, bm and f.
    */
  def map[A, B, C](am: M[A], bm: M[B])(f: (A, B) => C): M[C] = bind(am) { a => map(bm)(b => f(a, b)) }

  /**
    * The "lift" method which takes a function (A,B) => C and yields a function of (M[A], M[B]) => M[C].
    * Also known as "lift2"
    *
    * @param f a function (A,B) => C
    * @tparam A the underlying type of the first parameter of the resulting function
    * @tparam B the underlying type of the second parameter of the resulting function
    * @tparam C the underlying type of the result of the resulting function
    * @return an instance of (M[A], M[B]) => M[C] based on f.
    */
  def lift[A, B, C](f: (A, B) => C): (M[A], M[B]) => M[C] = map(_, _)(f)
}

object Monad {

  implicit object OptionMonad extends Monad[Option] {
    def unit[A](a: A) = Some(a)

    def bind[A, B](ao: Option[A])(f: A => Option[B]): Option[B] = ao flatMap f
  }

  implicit object TryMonad extends Monad[Try] {
    def unit[A](a: A) = Success(a)

    def bind[A, B](ay: Try[A])(f: A => Try[B]): Try[B] = ay flatMap f
  }

  implicit object ListMonad extends Monad[List] {
    def unit[A](a: A) = List(a)

    def bind[A, B](as: List[A])(f: A => List[B]): List[B] = as flatMap f
  }

  def sequence[M[_], A](ams: List[M[A]])(implicit mm: Monad[M]): M[List[A]] =
    ams.foldRight(mm.unit(List[A]())) { (am, asm) => mm.bind(am) { a => mm.bind(asm) { as => mm.unit(a :: as) } } }
}

trait Functor[F[_]] {
  /**
    * The "map" method which takes an M[A] and a function A => B and yields a M[B].
    *
    * @param af an instance of F[A]
    * @param f  a function A => B
    * @tparam A the underlying type of M[A]
    * @tparam B the underlying type of the result
    * @return an instance of M[B] based on am and f.
    */
  def map[A, B](af: F[A])(f: A => B): F[B]

  /**
    * The "lift" method which takes a function A => B and yields a function of M[A] => M[B].
    *
    * @param f a function A => B
    * @tparam A the underlying type of the parameter of the resulting function
    * @tparam B the underlying type of the result of the resulting function
    * @return an instance of M[A] => M[B] based on f.
    */
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
}

object Functor {

  implicit object ListFunctor extends Functor[List] {
    def map[A, B](as: List[A])(f: A => B): List[B] = as map f
  }

  implicit object OptionFunctor extends Functor[Option] {
    def map[A, B](ao: Option[A])(f: A => B): Option[B] = ao map f
  }

  implicit object TryFunctor extends Functor[Try] {
    def map[A, B](ay: Try[A])(f: A => B): Try[B] = ay map f
  }

}
