package edu.neu.coe.csye7200.fp

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

trait Foldable[F[_]] extends Functor[F] {
  def foldLeft[A, B](z: B)(f: (B, A) => B): B

  def foldRight[A, B](z: B)(f: (A, B) => B): B
}

//trait Foldable[F[_]] extends Functor[F] {
//  def foldRight[A,B](as: F[A])(z: B)(f: (A,B)=>B): B
//  def foldLeft [A,B](as: F[A])(z: B)(f: (B,A)=>B): B
//}
object Monoid {
  type IntList = List[Int]

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    override val zero = Nil
  }

  def foldableList = new Foldable[List] {

    def foldLeft[A, B](z: B)(f: (B, A) => B): B = ???

    // tail recursive
    def foldRight[A, B](z: B)(f: (A, B) => B): B = ???

    //  NOT tail recursive
    def reduce[A](f: (A, A) => A): A = ???

    def map[A, B](m: List[A])(f: A => B): List[B] = ???
  }
}

//trait Monad[F[_]] extends Functor[F] {
//  def unit[A](a: => A): F[A]
//  def flatMap[A,B](ma: F[A])(f: A=>F[B]): F[B]
//  def map[A,B](ma: F[A])(f: A=>B): F[B] = flatMap(ma)(a => unit(f(a)))
//  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A,B)=>C): F[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))
//}
