package fpinscala.monoids

/**
 * Created by tim on 15/08/13.
 */
trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoid {
  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2

    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2

    val zero = Nil
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(x: Option[A], y: Option[A]) = x orElse y

    val zero = None
  }

  def endoMonoid[A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A) = a1 compose a2

    val zero = (a: A) => a
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B) = as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) = foldMap(as, endoMonoid[B])(f.curried)(z)

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]) = new Monoid[(A, B)] {
    def op(a1: (A, B), a2: (A, B)) = A.op(a1._1, a2._1) -> B.op(a1._2, a2._2)

    def zero = (A.zero, B.zero)
  }

  def functionMonoid[A,B](B:Monoid[B]) = new Monoid[A=>B] {
    def op(f:A=>B,g:A=>B) = a => B.op(f(a),g(a))
    def zero = a=>B.zero
  }

}
