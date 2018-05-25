package parsing

import scala.language.postfixOps
import org.scalatest.FlatSpec

class ParserSpec extends FlatSpec {

  import parsing.Parser._

  "parser" should "parse a single character" in {
    assert('a'.parse('a'.toString).get === 'a')
    assert('b'.parse('b'.toString).get === 'b')
    assert('c'.parse('c'.toString).get === 'c')
  }

  "parser" should "parse a single string" in {
    assert("aa".parse("aa").get === "aa")
    assert("abc".parse("abc").get === "abc")
    assert("ccc".parse("ccc").get === "ccc")
  }

  "parser" should "have valid andThen combinator" in {
    assert(('a' andThen 'b').parse("ab").get === ('a', 'b'))
    assert(('a' andThen "bc").parse("abc").get === ('a', "bc"))
    assert(("ab" ~ 'c').parse("abc").get === ("ab", 'c'))
    assert(("aa" ~ "bb").parse("aabb").get === ("aa", "bb"))
  }

  "parser" should "have valid orElse combinator" in {
    assert(('a' orElse 'b').parse('a').get === 'a')
    assert(('a' orElse 'b').parse('b').get === 'b')
    assert(('a' orElse "bc").parse('a').get === 'a')
    assert(('a' orElse "bc").parse("bc").get === "bc")
    assert(("ab" | 'c').parse("ab").get === "ab")
    assert(("ab" | 'c').parse('c').get === 'c')
    assert(("ab" | "cd").parse("ab").get === "ab")
    assert(("ab" | "cd").parse("cd").get === "cd")
  }

  "parser" should "have valid repeat combinator" in {
    assert(repeat('a').parse("").get === List())
    assert(repeat('a').parse("aaa").get === List('a', 'a', 'a'))
    assert(repeat('a' andThen 'b').parse("abab").get === List(('a', 'b'), ('a', 'b')))
    assert(repeat("abc").parse("abcabc").get === List("abc", "abc"))
    assert(repeat("ab" ~ "cd").parse("abcdabcd").get === List(("ab", "cd"), ("ab", "cd")))
    assert(repeat("ab" | "cd").parse("abcd").get === List("ab", "cd"))
    assert(repeat("ab" | "cd").parse("cdab").get === List("cd", "ab"))
    for (i <- 1 to 10) {
      assert(repeat('a').parse(List.fill(i)('a').mkString).get === List.fill(i)('a'))
    }
  }

  "parser" should "have valid repeatN combinator" in {
    assert(repeatN(0)('a').parse("").get === List())
    assert(repeatN(3)('a').parse("aaa").get === List('a', 'a', 'a'))
    assert(repeatN(2)('a' andThen 'b').parse("abab").get === List(('a', 'b'), ('a', 'b')))
    assert(repeatN(2)("abc").parse("abcabc").get === List("abc", "abc"))
    assert(repeatN(2)("ab" ~ "cd").parse("abcdabcd").get === List(("ab", "cd"), ("ab", "cd")))
    assert(repeatN(2)("ab" | "cd").parse("abcd").get === List("ab", "cd"))
    assert(repeatN(2)("ab" | "cd").parse("cdab").get === List("cd", "ab"))
    for (i <- 1 to 10) {
      assert(repeatN(i)('a').parse(List.fill(i)('a').mkString).get === List.fill(i)('a'))
    }
  }

  "parser" should "have valid map combinator" in {
    assert((repeat('a') map (_.size)).parse("aaa").get === 3)
    assert((repeat('a' andThen 'b') map (_.size)).parse("abab").get === 2)
    assert((repeat("abc") map (_.mkString(","))).parse("abcabc").get === "abc,abc")
    assert((repeat("ab" ~ "cd") map (_.size)).parse("abcdabcd").get === 2)
    assert((repeat("ab" | "cd") map (_.tail)).parse("abcd").get === List("cd"))
    assert((repeat("ab" | "cd") map (_.head)).parse("cdab").get === "cd")
    for (i <- 1 to 10) {
      assert((repeat('a') map (_.size)).parse(List.fill(i)('a').mkString).get === i)
      assert((repeat('a') map (_.mkString("-"))).parse(List.fill(i)('a').mkString).get === List.fill(i)('a').mkString("-"))
      assert((repeat('a') map (_.take(5))).parse(List.fill(i)('a').mkString).get === List.fill(if (i > 5) 5 else i)('a'))
    }
  }

  "parser" should "have valid digit and digits parser" in {
    for (i <- 0 until 10) {
      assert(digit.parse(i.toString).get === i)
    }
    for (i <- 1 to 50) {
      for (j <- 51 to 100) {
        assert(digits.parse((i * j).toString).get === i * j)
      }
    }
  }

  "parser" should "have valid regex parser" in {
    val word: Parser[String] = "\\w+".r
    val date: Parser[String] = "(\\d{4})-(\\d{2})-(\\d{2})".r
    val time: Parser[String] = "(\\d{2}):(\\d{2}):(\\d{2})".r
    val email: Parser[String] = "([A-Za-z0-9]+)@([A-Za-z0-9]+)\\.([A-Za-z]{2,})".r
    assert(spaces.parse("     ").get === "     ")
    assert(spaces.parse(" \t \n \t ").get === " \t \n \t ")
    assert((spaces map (_.length)).parse("  \n\n  \t\t  ").get === 10)
    assert((spaces map (_.trim)).parse("  \n\n  \t\t  ").get === "")
    assert(word.parse("hello").get === "hello")
    assert(date.parse("2018-04-01").get === "2018-04-01")
    assert(time.parse("12:00:00").get === "12:00:00")
    assert(email.parse("p001@nyu.edu").get === "p001@nyu.edu")
    val dateTime = date ~ spaces ~ time
    assert(dateTime.parse("2018-04-01 12:00:00").get === (("2018-04-01", " "), "12:00:00"))
  }

  "parser" should "have valid flatMap combinator" in {
    assert((digit flatMap (repeatN(_)('a'))).parse("3aaa").get === List('a', 'a', 'a'))
    for (i <- 0 until 100) {
      assert((digits flatMap (repeatN(_)('a'))).parse(i.toString + (List.fill(i)('a').mkString)).get === List.fill(i)('a'))
    }
    assert((repeat('a') map (_.size) flatMap (_.toString)).parse("aaa3").get === "3")
    for (i <- 0 until 100) {
      assert((repeat('a') map (_.size) flatMap (_.toString)).parse((List.fill(i)('a').mkString) + i.toString).get === i.toString)
    }
    assert((repeat('a') map (_.size * 2) flatMap (repeatN(_)('b')) map (_.mkString)).parse("aabbbb").get === "bbbb")
    assert((digit flatMap (x => (x + 1).toString) flatMap (x => (x.toInt + 1).toString)).parse("234").get === "4")
    assert((("aa" | "bbb") flatMap (x => repeatN(x.length)('c')) flatMap (_.mkString(",")) map (_.size)).parse("aaccc,c").get === 3)
    assert((("aa" | "bbb") flatMap (x => repeatN(x.length)('c')) flatMap (_.mkString(",")) map (_.size)).parse("bbbcccc,c,c").get === 5)
  }

  "parser" should "have valid attempt parser" in {
    assert((attempt('a' andThen 'a') orElse ('a' andThen 'b')).parse("ab").get === ('a', 'b'))
    assert((attempt("ab" ~ "ba") | ("ab" ~ "cd")).parse("abcd").get === ("ab", "cd"))
    assert((attempt("ab" ~ "ba") | attempt("ab" ~ "cd") | ("cd" ~ "ab")).parse("cdab").get === ("cd", "ab"))
    for (i <- 1 to 10) {
      for (j <- 11 to 20) {
        assert((attempt(repeatN(i)('a') ~ repeatN(j)('b'))
          | attempt(repeatN(i)('b') ~ repeatN(j)('a'))
          | attempt(repeatN(j)('a') ~ repeatN(i)('b'))
          | (repeatN(j)('b') ~ repeatN(i)('a')))
          .parse(List.fill(j)('a').mkString + List.fill(i)('b').mkString)
          .get === (List.fill(j)('a'), List.fill(i)('b')))
      }
    }
  }

  "parser" should "work well with extended functionality" in {
    assert((digits <~ spaces).parse("123   ").get === 123)
    assert((digits <~ spaces <~ digits).parse("123   456").get === 123)
    assert((spaces ~> digits).parse("   123").get === 123)
    assert((digits ~> spaces ~> digits).parse("456   123").get === 123)
    assert((spaces ~> digits <~ spaces).parse("   123   ").get === 123)
    assert((digits <~ (spaces ~> digits)).parse("123   456").get === 123)
    assert(((digits <~ spaces) ~> digits).parse("123   456").get === 456)
    assert((digits ~> spaces ~> digits <~ spaces <~ digits).parse("12  34  56").get === 34)
    def digitN(n: Int): Parser[Int] = if (n == 1) digit else digitN(n - 1) <~ digit
    for (i <- 1 until 10) {
      assert((digitN(i) ~> digits).parse(Integer.MAX_VALUE.toString).get === (Integer.MAX_VALUE.toString.reverse.toLong / Math.pow(10, i).toLong).toString.reverse.toInt)
    }
    assert(trim(digits)("abc").parse("123abc321").get === "abc")
    assert(trimSpaces(digits).parse("   123   ").get === 123)
    assert(trim(spaces)(trim(digits)("abc")).parse("   123abc321   ").get === "abc")
    assert(enclose('[', ']')(digit).parse("[2]").get === 2)
    assert(enclose("abc", "def")(digit).parse("abc2def").get === 2)
    assert(enclose(spaces, digits)("abc").parse("   abc123").get === "abc")
    assert((into(digits, "digits") | into(spaces, "spaces")).parse("123").get === "digits")
    assert((into(digits, "digits") | into(spaces, "spaces")).parse("   ").get === "spaces")
    assert(list(digits, spaces).parse("11 22 33 44 55 66").get === List(11, 22, 33, 44, 55, 66))
    assert(trimSpaces(list(digits, spaces)).parse(" 11 22 33 44 55 66 ").get === List(11, 22, 33, 44, 55, 66))
    val word: Parser[String] = "\\w+".r
    assert(list(word, spaces).parse("have a nice day").get.mkString(",") === "have,a,nice,day")
  }

  "parser" should "pass failure test" in {
    assert((('a' andThen 'b') | ('a' andThen 'a')).parse("aa").isFailure)
    assert(!(('a' andThen 'b') | ('b' andThen 'a')).parse("ba").isFailure)
    assert((('a' andThen "bc") | ('a' andThen "cd")).parse("acd").isFailure)
    assert(!(('a' andThen "bc") | ('b' andThen "cd")).parse("bcd").isFailure)
    assert((("aa" ~ "bb") | ("aa" ~ "cc")).parse("aacc").isFailure)
    assert(!(("aa" ~ "bb") | ("bb" ~ "aa")).parse("bbaa").isFailure)
    assert(!(("aa" ~ "cc") | ("ab" ~ "cc")).parse("abcc").isFailure)
    assert(((digit ~ digits) | (digit ~ spaces)).parse("1 ").isFailure)
    assert(((digit ~ digits) | (spaces ~ digits)).parse("1").isFailure)
    assert(!((digit ~ spaces) | (spaces ~ digits)).parse("12").isFailure)
    assert(!((digit ~ spaces) | (spaces ~ digit)).parse("12").isFailure)
  }

  def getErrMsg[A](p: Parser[A])(s: String) = intercept[Exception] { p.parse(s).get }.getMessage

  "parser" should "have valid label and tag combinator" in {
    val err1 = "Error! Number expected!"
    assert(getErrMsg(label(err1)(number))("abc") === err1)
    for (i <- 1 to 100) {
      assert(getErrMsg(label(i.toString)(number))(" " * i) === i.toString)
    }
    val err2 = "Error! Digit expected!"
    assert(getErrMsg(tag(err2)(tag(err1)(number)))("abc") === (err1 + err2))
    for (i <- 0 until 10) {
      for (j <- 50 until 60) {
        assert(getErrMsg(tag(j.toString)(tag(i.toString)(number)))(" " * i * j) === (i.toString + j.toString))
      }
    }
  }

}
