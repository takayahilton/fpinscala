package fpinscala.parsing

import org.scalacheck.{Prop, Gen, Properties}
import org.scalacheck.Prop.forAll
import scala.language.higherKinds
import scala.language.implicitConversions

import scala.util.matching.Regex

/**
 * 第９章　パーサーコンビネータ
 */


trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  def slice[A](p: Parser[A]): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = flatMap(p)(a => succeed(f(a)))

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = for {
    a <- p
    b <- p2
  } yield (f(a, b))


  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def many[A](p: Parser[A]): Parser[List[A]] = or(map2(p, many(p))(_ :: _), succeed(List()))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)


  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] = flatMap(p)(a => map(p2)(b => (a, b)))

  def succeed[A](a: A): Parser[A] = string("") map (_ => a)

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]

  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def map[B](f: A => B) = self.map(p)(f)

    def product[B](p2: Parser[B]) = self.product(p, p2)

    def **[B](p2: Parser[B]) = self.product(p, p2)

    def flatMap[B](f:A=>Parser[B]):Parser[B] = self.flatMap(p)(f)
  }

  object Laws {

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop = forAll(in) { s: String =>
      run(p1)(s) == run(p2)(s)
    }

    def mapLaw[A](p: Parser[A])(in: Gen[String]) = equal(p, p.map(a => a))(in)

    def productLow[A, B, C](p: Parser[A], p2: Parser[B], p3: Parser[C])(in: Gen[String]) = equal((p ** p2) ** p3, p ** (p2 ** p3))(in)

  }


}
