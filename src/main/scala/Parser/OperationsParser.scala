package Parser


import scala.util.parsing.combinator.RegexParsers


object OperationsParser extends RegexParsers {

  def number: Parser[Int] = """\d+""".r ^^ {
    _.toInt
  }

  def factor: Parser[Int] =  "(" ~> expr <~ ")" | number

  def term: Parser[Int] = factor ~ rep("*" ~ factor | "/" ~ factor) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (a, "*" ~ b) => a * b
      case (a, "/" ~ b) => a / b
    }
  }

  def expr: Parser[Int] = term ~ rep("+" ~ term | "-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number) {
      case (a, "+" ~ b) => a + b
      case (a, "-" ~ b) => a - b
    }
  }

  def apply(input: String) = parseAll(expr, input)


}


