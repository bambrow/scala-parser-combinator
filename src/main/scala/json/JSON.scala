package json

/** Provides essential parsing for JSON String input.
  * Input will be parsed into an abstract syntax tree.
  *
  * ==Overview==
  * The main class to use is [[json.JSON]], as so
  * {{{
  *   JSON.parse("{ "x": 1 }")
  * }}}
  */

import parsing.Parser

object JSON {

  /** Specifies the basic objects for different JSON data.
    * Used by {{{case object JNull, case class JBool, case class JNumber, case class JString, case class JArray, case class JObject}}}
    * */
  sealed abstract class JSON

  case object JNull extends JSON
  case class JBool(b: Boolean) extends JSON
  case class JNumber(n: Double) extends JSON
  case class JString(s: String) extends JSON
  case class JArray(a: IndexedSeq[JSON]) extends JSON
  case class JObject(a: Map[String, JSON]) extends JSON

  /**
    * Provides basic JSON parser for parsing input JSON string.
    * @return  parsed JSON data into abstract syntax tree
    */
  def jsonParser: Parser[JSON] = {

    import parsing.Parser._

    val jsonNull = into("null", JNull)

    val jsonBool = into("true", JBool(true)) | into("false", JBool(false))

    val jsonNumber = attempt(number map (JNumber(_)))

    val quotedStr = attempt(trimSpaces(('"') ~> ("""((?:[^"\\]|\\[\\"/bfnrtu])*)""".r) <~ label("expected '\"'")('"')))

    val jsonString = quotedStr map (str => JString(mapControl(str.toString)))

    lazy val keyValuePair = (quotedStr map (mapControl(_))) ~ (label("expected ':'")(':') ~> (jsonValue))

    lazy val jsonObject = ((trimSpaces('{') ~> list(keyValuePair, ',') <~ mapFailure("expected '}'")(trimSpaces('}'))) map { pairs => JObject(pairs.toMap) })

    lazy val jsonArray = (trimSpaces('[') ~> (list(jsonValue, ',') map { values => JArray(values.toIndexedSeq) }) <~ mapFailure("expected ']'")(trimSpaces(']')))

    lazy val jsonValue: Parser[JSON] = mapFailure("illegal start of JSON value")(trimSpaces(jsonNull | jsonBool | jsonNumber | jsonString | jsonArray | jsonObject))

    tagError(jsonValue <~ label("there should be no trailing characters")(eof))
  }

  /**
    * Parse the string input based upon the parser object.
    * @param s   string input
    * @return    abstract syntax tree based upon the input
    */
  def parse(s: String): JSON = jsonParser.parse(s).get

  /**
    * Handle control characters in JSON string.
    * @param str   input string
    * @return     control characters mapped
    */
  def mapControl(str: String): String = {
    str.replaceAll("""\\"""", """"""")
       .replaceAll("""\\\\""", """\\""")
       .replaceAll("""\\/""", """/""")
       .replaceAll("""\\b""", "\b")
       .replaceAll("""\\f""", "\f")
       .replaceAll("""\\n""", "\n")
       .replaceAll("""\\r""", "\r")
       .replaceAll("""\\t""", "\t")
  }

}