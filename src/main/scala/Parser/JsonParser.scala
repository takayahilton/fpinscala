package Parser

import scala.util.parsing.combinator.RegexParsers


object JsonParser extends RegexParsers {

  type Key = String

  sealed trait Json

  case class JObject(get: Option[Map[Key, Json]]) extends Json

  case class JArray(get: Option[IndexedSeq[Json]]) extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case object JNul extends Json

  case object JUndefined extends Json

  def string: Parser[JString] = ("\".?\"" | "'.?'".r) ^^ {
    s => JString(s)
  }

  def number: Parser[JNumber] = """[-]?\d+(\.\d+)?""".r ^^ {
    n => JNumber(n.toDouble)
  }

  def member: Parser[Map[Key, Json]] = string ~ ":" ~ json ^^ {
    case key ~ ":" ~ json => Map(key.get -> json)
  }

  def json: Parser[Json] = string | number | obj

  def obj: Parser[JObject] =  "{" ~> member <~ "}" ^^ {
    case member => JObject(Some(member))
  }


  def apply(input: String) = parseAll(json, input)


}
