package json


import scala.language.postfixOps
import org.scalatest.FlatSpec
import JSON._

class JSONSpec extends FlatSpec {

  import parsing.Parser._

  val testInput =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello",
      |  "array": [{ "x": 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin
  
  val expected = JObject (
    Map (
      "number" -> JNumber(3.0),
      "bool" -> JBool(true),
      "string" -> JString("Hello"),
      "array" -> JArray(IndexedSeq(JObject(Map("x" -> JNumber(3))), JNumber(42), JNull)),
      "object" -> JObject(Map())
    )
  )
  
  "jsonParser" should "parse JSON data" in {
    assert(JSON.parse(testInput) === expected)
  }
  
  "jsonParser" should "parse JSON number" in {
    assert(JSON.parse("{\"number\":1}") === JObject( Map( "number" -> JNumber(1.0) ) ))
    assert(JSON.parse("{\"number\":1.0}") === JObject( Map( "number" -> JNumber(1.0) ) ))
    assert(JSON.parse("{\"number\":1.000}") === JObject( Map( "number" -> JNumber(1.0) ) ))
    assert(JSON.parse("{\"number\":-1}") === JObject( Map( "number" -> JNumber(-1.0) ) ))
    assert(JSON.parse("{\"number\":-1.0}") === JObject( Map( "number" -> JNumber(-1.0) ) ))
    assert(JSON.parse("{\"number\":-1.000}") === JObject( Map( "number" -> JNumber(-1.0) ) ))
    assert(JSON.parse("{\"number\":123}") === JObject( Map( "number" -> JNumber(123.0) ) ))
    assert(JSON.parse("{\"number\":123.45}") === JObject( Map( "number" -> JNumber(123.45) ) ))
    assert(JSON.parse("{\"number\":1e2}") === JObject( Map( "number" -> JNumber(100.0) ) ))
    assert(JSON.parse("{\"number\":1E2}") === JObject( Map( "number" -> JNumber(100.0) ) ))
    assert(JSON.parse("{\"number\":1e+2}") === JObject( Map( "number" -> JNumber(100.0) ) ))
    assert(JSON.parse("{\"number\":1E+2}") === JObject( Map( "number" -> JNumber(100.0) ) ))
    assert(JSON.parse("{\"number\":1e-2}") === JObject( Map( "number" -> JNumber(0.01) ) ))
    assert(JSON.parse("{\"number\":1E-2}") === JObject( Map( "number" -> JNumber(0.01) ) ))
    assert(JSON.parse("{\"number\":-1.5e+3}") === JObject( Map( "number" -> JNumber(-1500.0) ) ))
    assert(JSON.parse("{\"number\":-0.5E2}") === JObject( Map( "number" -> JNumber(-50.0) ) ))
    assert(JSON.parse("{ \"number\" : -0.5E2 }") === JObject( Map( "number" -> JNumber(-50.0) ) ))
    assert(JSON.parse("{\t\"number\"\t:   -0.5E2   }") === JObject( Map( "number" -> JNumber(-50.0) ) ))
    assert(JSON.parse("\n{\n\"number\"\n:\n-0.5E2\n}\n") === JObject( Map( "number" -> JNumber(-50.0) ) ))
    assert(JSON.parse(
      """{
        |  "number": -0.5E2
        |}
      """.stripMargin) === JObject( Map( "number" -> JNumber(-50.0) ) ))
  }

  "jsonParser" should "parse JSON boolean" in {
    assert(JSON.parse("{\"bool\":true}") === JObject( Map( "bool" -> JBool(true) ) ))
    assert(JSON.parse("{\"bool\":false}") === JObject( Map( "bool" -> JBool(false) ) ))
    assert(JSON.parse("{\"bool\": true}") === JObject( Map( "bool" -> JBool(true) ) ))
    assert(JSON.parse("{\"bool\": false}") === JObject( Map( "bool" -> JBool(false) ) ))
    assert(JSON.parse("{ \"bool\" : true }") === JObject( Map( "bool" -> JBool(true) ) ))
    assert(JSON.parse("{ \"bool\" : false }") === JObject( Map( "bool" -> JBool(false) ) ))
    assert(JSON.parse("{\t\"bool\"\n:\ntrue\t}") === JObject( Map( "bool" -> JBool(true) ) ))
    assert(JSON.parse("{\t\"bool\"\n:\nfalse\t}") === JObject( Map( "bool" -> JBool(false) ) ))
    assert(JSON.parse(
      """{
        |  "bool": true
        |}
      """.stripMargin) === JObject( Map( "bool" -> JBool(true) ) ))
  }

  "jsonParser" should "parse JSON null" in {
    assert(JSON.parse("{\"null\":null}") === JObject( Map( "null" -> JNull ) ))
    assert(JSON.parse("{\"null\": null }") === JObject( Map( "null" -> JNull ) ))
    assert(JSON.parse("{ \"null\" : null }") === JObject( Map( "null" -> JNull ) ))
    assert(JSON.parse("{\n\n\"null\"\n\n:\n\nnull\n\n}") === JObject( Map( "null" -> JNull ) ))
    assert(JSON.parse(
      """{
        |  "null": null
        |}
      """.stripMargin) === JObject( Map( "null" -> JNull ) ))
  }

  "jsonParser" should "parse JSON string" in {
    assert(JSON.parse(
      """{
        |  "string": ""
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("") ) ))
    assert(JSON.parse(
      """{
        |  "string": "abc"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("abc") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\""
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\"") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\\"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\\") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\/"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("/") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\b"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\b") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\f"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\f") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\n"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\n") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\r"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\r") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\t"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("\t") ) ))
    assert(JSON.parse(
      """{
        |  "string": "\u0041"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("A") ) ))
    assert(JSON.parse(
      """{
        |  "string": "hello\nworld"
        |}
      """.stripMargin) === JObject( Map( "string" -> JString("hello\nworld") ) ))
  }

  "jsonParser" should "parse JSON array" in {
    assert(JSON.parse(
      """{
        |  "array": []
        |}
      """.stripMargin) === JObject( Map( "array" -> JArray(IndexedSeq()) ) ))
    assert(JSON.parse(
      """{
        |  "array": [1,2,3]
        |}
      """.stripMargin) === JObject( Map( "array" -> JArray(IndexedSeq(JNumber(1.0), JNumber(2.0), JNumber(3.0))) ) ))
    assert(JSON.parse(
      """{
        |  "array": [null, null]
        |}
      """.stripMargin) === JObject( Map( "array" -> JArray(IndexedSeq(JNull, JNull)) ) ))
    assert(JSON.parse(
      """{
        |  "array": [true, null, false]
        |}
      """.stripMargin) === JObject( Map( "array" -> JArray(IndexedSeq(JBool(true), JNull, JBool(false))) ) ))
    assert(JSON.parse(
      """{
        |  "array": [[[], []]]
        |}
      """.stripMargin) === JObject( Map( "array" -> JArray(IndexedSeq(JArray(IndexedSeq(JArray(IndexedSeq()), JArray(IndexedSeq()))))) ) ))
    assert(JSON.parse(
      """{
        |  "array": [[[[[]]]]]
        |}
      """.stripMargin) === JObject( Map( "array" -> JArray(IndexedSeq(JArray(IndexedSeq(JArray(IndexedSeq(JArray(IndexedSeq(JArray(IndexedSeq()))))))))) ) ))
  }

  "jsonParser" should "parse JSON object" in {
    assert(JSON.parse(
      """{
        |  "object": {}
        |}
      """.stripMargin) === JObject( Map( "object" -> JObject(Map()) ) ))
    assert(JSON.parse(
      """{
        |  "object": {
        |    "a": 1
        |  }
        |}
      """.stripMargin) === JObject( Map( "object" -> JObject(Map( "a" -> JNumber(1.0) )) ) ))
    assert(JSON.parse(
      """{
        |  "object": {
        |    "a": {}
        |  }
        |}
      """.stripMargin) === JObject( Map( "object" -> JObject(Map( "a" -> JObject(Map()) )) ) ))
    assert(JSON.parse(
      """{
        |  "object": {
        |    "a": [{}]
        |  }
        |}
      """.stripMargin) === JObject( Map( "object" -> JObject(Map( "a" -> JArray(IndexedSeq(JObject(Map()))) )) ) ))
    assert(JSON.parse(
      """{
        |  "object": {
        |    "a": {
        |      "b": {
        |        "c": 1
        |      }
        |    }
        |  }
        |}
      """.stripMargin) === JObject( Map( "object" -> JObject(Map( "a" -> JObject(Map( "b" -> JObject(Map( "c" -> JNumber(1.0) )) )) )) ) ))
  }

  "jsonParser" should "parse simple JSON data" in {
    assert(JSON.parse("1") === JNumber(1.0) )
    assert(JSON.parse("null") === JNull )
    assert(JSON.parse("true") === JBool(true) )
    assert(JSON.parse("\"a\"") === JString("a") )
    assert(JSON.parse("[]") === JArray(IndexedSeq()) )
    assert(JSON.parse("{}") === JObject(Map()) )
  }

  val str1 =
    """{ "null": null, "bool": true, "number": 2.0, "string": "hello", "array": [], "object": {} }""".stripMargin

  val test1 = str1

  val map1 = Map(
    "null" -> JNull,
    "bool" -> JBool(true),
    "number" -> JNumber(2.0),
    "string" -> JString("hello"),
    "array" -> JArray(IndexedSeq()),
    "object" -> JObject(Map())
  )

  val ans1 = JObject(map1)

  val test2 =
    """{ "null": null, "bool": true, "number": 2.0, "string": "hello", "array": [], "object": """.stripMargin + str1 + " }"

  val ans2 = JObject(map1 + ("object" -> JObject(map1)))

  val test3 =
    """{ "null": null, "bool": true, "object": """.stripMargin + str1 + " }"

  val ans3 = JObject(map1 + ("object" -> JObject(map1)) - "number" - "string" - "array")

  val test4 = """{ "null": null, "bool": true, "number": 2.0, "string": "hello", "array": [ """.stripMargin + str1 + "," + str1 + "," + str1 + " ] }"

  val ans4 = JObject(map1 - "object" + ("array" -> JArray(IndexedSeq(JObject(map1), JObject(map1), JObject(map1)))))

  val test5 = """ { "single member object" : ["single member array"] } """.stripMargin

  val ans5 = JObject(Map("single member object" -> JArray(IndexedSeq(JString("single member array")))))

  val test6 = """ [ "empty test", [], {}, null ] """.stripMargin

  val ans6 = JArray(IndexedSeq(JString("empty test"), JArray(IndexedSeq()), JObject(Map()), JNull))

  val test7 = """ { "" : "" } """.stripMargin

  val ans7 = JObject(Map("" -> JString("")))

  val test8 = """ { "control test" : "\"\\\/\b\f\n\r\t" } """.stripMargin

  val ans8 = JObject(Map("control test" -> JString("\"\\/\b\f\n\r\t")))

  val test9 = """ { "special test" : "`~!@#$%^&*()-=_+{}[]|:';<>,./?" } """.stripMargin

  val ans9 = JObject(Map("special test" -> JString("`~!@#$%^&*()-=_+{}[]|:';<>,./?")))

  val test10 = """ { "unicode test" : "\u0123\u0def\u1234\u28ff\u2eee\u4e00\u5678" } """.stripMargin

  val ans10 = JObject(Map("unicode test" -> JString("\u0123\u0def\u1234\u28ff\u2eee\u4e00\u5678")))

  val test11 = """ { "url test" : "https://www.google.com" } """.stripMargin

  val ans11 = JObject(Map("url test" -> JString("https://www.google.com")))

  val test12 =
    """ { "space test" : [ 1,     2   ,
      |  3
      |    ,
      |    4    ,5,
      |              6
      |
      |
      |
      |              ,
      |              7,
      |                          8
      |
      |
      |
      |                          ,
      |
      |
      |
      |
      |                              9] } """.stripMargin

  val test13 = """ { "space test" : [1,2,3,4,5,6,7,8,9] } """.stripMargin

  val test14 = """ { "json test" : { "single member object" : ["single member array"] } } """.stripMargin

  val ans14 = JObject(Map("json test" -> JObject(Map("single member object" -> JArray(IndexedSeq(JString("single member array")))))))

  val test15 = """ { "\\\/\b\f\n\r\t`~!@#$%^&*()-=_+{}[]|:';<>,./?\u0123\u0def\u1234\u28ff\u2eee\u4e00\u5678" : "key test" } """.stripMargin

  val ans15 = JObject(Map("\\/\b\f\n\r\t`~!@#$%^&*()-=_+{}[]|:';<>,./?\u0123\u0def\u1234\u28ff\u2eee\u4e00\u5678" -> JString("key test")))

  val test16 = """ { "quote test" : { "\"" : "\"" } } """.stripMargin

  val ans16 = JObject(Map("quote test" -> JObject(Map("\"" -> JString("\"")))))

  val test17 = """ { "multiple quote test" : { "\"\"\"\"\"" : "\"\"\"\"\"" } } """.stripMargin

  val ans17 = JObject(Map("multiple quote test" -> JObject(Map("\"\"\"\"\"" -> JString("\"\"\"\"\"")))))

  "jsonParser" should "parse complicated JSON data" in {
    assert(JSON.parse(test1) === ans1)
    assert(JSON.parse(test2) === ans2)
    assert(JSON.parse(test3) === ans3)
    assert(JSON.parse(test4) === ans4)
    assert(JSON.parse(test5) === ans5)
    assert(JSON.parse(test6) === ans6)
    assert(JSON.parse(test7) === ans7)
    assert(JSON.parse(test8) === ans8)
    assert(JSON.parse(test9) === ans9)
    assert(JSON.parse(test10) === ans10)
    assert(JSON.parse(test11) === ans11)
    assert(JSON.parse(test12) === JSON.parse(test13))
    assert(JSON.parse(test14) === ans14)
    assert(JSON.parse(test15) === ans15)
    assert(JSON.parse(test16) === ans16)
    assert(JSON.parse(test17) === ans17)
  }

  val err1 =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string" "Hello",
      |  "array": [{ "x": 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin

  val err2 =
    """{
      |  "number": 3.0,
      |  "bool": ,
      |  "string": "Hello",
      |  "array": [{ "x": 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin

  val err3 =
    """{
      |  "number": 3f,
      |  "bool": true,
      |  "string": "Hello",
      |  "array": [{ "x": 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin

  val err4 =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello",
      |  "array": [{ "x": 3 }, 42, null,
      |  "object": {}
      |}
    """.stripMargin

  val err5 =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello",
      |  "array": [{ "x": 3 , 42, null],
      |  "object": {}
      |}
    """.stripMargin

  val err6 =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello",
      |  "array": [{ x: 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin

  val err7 =
    """{
      |  "number": 3.0,
      |  "bool": true,
      |  "string": "Hello,
      |  "array": [{ "x": 3 }, 42, null],
      |  "object": {}
      |}
    """.stripMargin

  def printErrMsg(s: String) = println ( intercept[Exception] { JSON parse s }.getMessage )

  "jsonParser" should "report correct error message" in {
    printErrMsg("")
    printErrMsg("error")
    printErrMsg("1f")
    printErrMsg("true 1")
    printErrMsg("null true")
    printErrMsg("""["missing second element in array",]""")
    printErrMsg("""[,"missing first element in array"]""")
    printErrMsg("""["missing the second bracket"""")
    printErrMsg(""""missing the first bracket"]""")
    printErrMsg("""["extra comma after array"],""")
    printErrMsg(""",["extra comma before array"]""")
    printErrMsg("""[ "no" "comma" ]""")
    printErrMsg("""[ "wrong" : "comma" ]""")
    printErrMsg("""[ "wrong" . "comma" ]""")
    printErrMsg("""[ 'single quote element' ]""")
    printErrMsg("""[ "not paired" }""")
    printErrMsg("""{ error : "key is not quoted" }""")
    printErrMsg("""{ "error" : "extra comma after element", }""")
    printErrMsg("""{ ,"error" : "extra comma before element" }""")
    printErrMsg("""{ "error" : "double value" "double value" }""")
    printErrMsg("""{ "error" : "misplaced" } "it is true" """)
    printErrMsg("""{ "illegal value" : hello }""")
    printErrMsg("""{ "illegal value" : true hello }""")
    printErrMsg("""{ "illegal value" : hello true }""")
    printErrMsg("""{ "illegal value" : 1+2 }""")
    printErrMsg("""{ "illegal value" : hello }""")
    printErrMsg("""{ "illegal number" : 01 }""")
    printErrMsg("""{ "illegal number" : 0x36 }""")
    printErrMsg("""{ "no" "colon" }""")
    printErrMsg("""{ "extra" :: "colon" }""")
    printErrMsg("""{ "wrong" , "colon" }""")
    printErrMsg("""{ "wrong" . "colon" }""")
    printErrMsg("""{ "not paired": true ]""")
    printErrMsg(err1)
    printErrMsg(err2)
    printErrMsg(err3)
    printErrMsg(err4)
    printErrMsg(err5)
    printErrMsg(err6)
    printErrMsg(err7)
  }

}