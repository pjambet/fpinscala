package fpinscala.parsing

import scala.language.{higherKinds, implicitConversions}

trait JSON

object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def jsonParser[Unit, Parser[+ _]](P: Parsers[Parser]): Parser[JSON] = {
    import P.{string => _, _}
    implicit def tok(s: String): Parser[String] = token(P.string(s))

    def obj: Parser[JObject] = surround("{", "}")(
      keyvalue.sep(",").map(vs => JObject(vs.toMap)).scope("object")
    )
    def arr: Parser[JArray] = surround("[", "]")(
      value.sep(",").map(vs => JArray(vs.toIndexedSeq)).scope("array")
    )
    def lit: Parser[JSON] = scope("literal") {
      "null".as(JNull) |
        double.map(JNumber) |
        escapedQuoted.map(JString) |
        "true".as(JBool(true)) |
        "false".as(JBool(false))
    }
    def keyvalue: Parser[(String, JSON)] = {
      val r = escapedQuoted ** ((":" | ";") *> value)
      println(r)
      r
    }
    def value: Parser[JSON] = lit | obj | arr

//    val spaces = char(' ').many.slice
    root(whitespace *> (obj | arr))
  }
}

/**
  * JSON parsing example.
  */
object JSONExample {

  def test = {
    val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9 ,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

    val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

    val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""
//    val P = fpinscala.parsing.Reference
    val P = fpinscala.parsing.MyParser
//    import fpinscala.parsing.ReferenceTypes.Parser
    import fpinscala.parsing.MyParserTypes.Parser

    def printResult[E](e: Either[E, JSON]): Unit =
      e.fold(println, println)

    val json: Parser[JSON] = JSON.jsonParser[Unit, Parser](P)
//    printResult { P.run(json)(jsonTxt) }
//    println("--")
    printResult { P.run(json)(malformedJson1) }
//    println("--")
//    printResult { P.run(json)(malformedJson2) }
  }
}
