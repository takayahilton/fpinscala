package fpinscala.monads

import scala.language.higherKinds

/**
 * Created by tim on 15/08/13.
 */
trait Monad[F[_]] {

  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] = flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma) {
    a => map(mb)(b => (f(a, b)))
  }

  def sequence[A](lma: List[F[A]]): F[List[A]] = lma match {
    case Nil => unit(Nil)
    case head :: tail => flatMap(head) {
      a => map(sequence(tail))(la => a :: la)
    }
  }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(b => g(b))

  def flatMap_[A, B](ma: F[A])(f: A => F[B]): F[B] = compose((_: Unit) => ma, f)(())

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(a => a)

}


object Monad {

  val listMonad = new Monad[List] {
    def unit[A](a: => A) = List(a)

    def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma.flatMap(f)
  }

  val optionMonad = new Monad[Option] {
    def unit[A](a: => A) = Option(a)

    def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma.flatMap(f)
  }


}