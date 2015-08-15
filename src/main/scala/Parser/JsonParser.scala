package Parser

import scala.util.parsing.combinator.RegexParsers


object JsonParser extends RegexParsers {

  type Key = String

  sealed trait Json

  case class JObject(get: Map[Key, Json]) extends Json

  case class JArray(get: IndexedSeq[Json]) extends Json

  case class JNumber(get: Double) extends Json

  case class JString(get: String) extends Json

  case object JNul extends Json

  case object JUndefined extends Json

  def string: Parser[JString] = "'" ~> "[^']*".r <~ "'" ^^ {
    s => JString(s)
  }

  def number: Parser[JNumber] = """[-]?\d+(\.\d+)?""".r ^^ {
    n => JNumber(n.toDouble)
  }

  def jnull: Parser[JNul.type] = "null" ^^ { n => JNul}

  def member: Parser[Map[Key, Json]] = string ~ ":" ~ json ^^ {
    case key ~ ":" ~ json => Map(key.get -> json)
  }

  def json: Parser[Json] = string | number | obj | arr | jnull

  def obj: Parser[JObject] = "{" ~> repsep(  member  , "," )  <~ "}" ^^ {
    case  list => JObject(
      list.foldLeft(Map[Key,Json]()){
      (b,a)=>b ++ a
    })
  }

  def arr:Parser[JArray] = "[" ~> repsep(json , ",")  <~ "]"  ^^{
    case list => JArray(list.toIndexedSeq)
  }


  def apply(input: String) = parseAll(json, input)


}
