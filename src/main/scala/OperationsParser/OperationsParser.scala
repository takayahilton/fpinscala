package OperationsParser


import scala.io.Source
import scala.util.parsing.combinator.RegexParsers


object SParser extends RegexParsers {

  def number:Parser[Int] = """\d+""".r ^^{
    _.toInt
  }

  def factor:Parser[Int] = number | "(" ~> expr <~")"

  def term:Parser[Int] = factor ~ rep("*" ~factor |"/" ~ factor) ^^{
    case number ~ list => list.foldLeft(number){
      case (a,"*" ~ b) => a * b
      case (a,"/" ~b) => a / b
    }
  }

  def expr:Parser[Int] = term ~ rep("+" ~ term|"-" ~ term) ^^ {
    case number ~ list => list.foldLeft(number){
      case (a,"+" ~ b) => a+b
      case (a,"-" ~ b) => a-b
    }
  }

  def apply(input:String) = parseAll(expr,input)


}


