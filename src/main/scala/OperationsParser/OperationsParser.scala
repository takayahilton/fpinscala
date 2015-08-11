package OperationsParser


import scala.io.Source
import scala.util.parsing.combinator.RegexParsers


object OperationsParser extends RegexParsers {

  def number: Parser[Double] = """\d+(\.\d*)?""".r ^^ {
    _.toDouble
  }

  def factor: Parser[Double] = number | "(" ~> expr <~ ")"

  def term: Parser[Double] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (x, "*" ~ y) => x * y
      case (x, "/" ~ y) => x / y
    }
  }

  def expr:Parser[Double] = term ~ rep("+" ~ expr |"-" ~ expr )^^{
    case number ~ list => list.foldLeft(number){
      case (x, "+" ~ y) => x + y
      case (x, "-" ~ y) => x - y
    }
  }

  def apply(input: String) = parseAll(expr, input)

}


