package parsing

/** Provides essential combinators for char sequence based parsing. Also provides
  * implicits for converting from String, Char and regex to parsers.
  *
  * ==Overview==
  * The main class to use is [[parsing.Parser]], as so
  * {{{
  *   val strParserNull: Parser[String] = "string"
  * }}}
  */

import scala.language.implicitConversions
import scala.util.{Success, Try, Failure}
import scala.util.matching.Regex
import scala.annotation.tailrec

/** Specifies the basic operations that parsing locations should support.
  * Used by {{{case class StringLocation}}}
  * */

trait Location {
  def advance(n: Int): Location

  def getLength: Int

  def toString: String

  def getLoc: Int
}

/** Provides the implementation of the operations
  *
  * @param storage the string to be parsed
  * @param loc records the parsing location in the string
  */
case class StringLocation(storage: String, loc: Int = 0) extends Location {
  /** advance n when parser consumes n characters
    *
    * @param n character number to advance
    * @return a StringLocation object with the new location
    */
  override def advance(n: Int): Location = StringLocation(storage, loc + n)

  /** Get the length of the string
    *
    * @return length of the string
    */
  override def getLength: Int = storage.length

  /** Provides information for parsing location
    *
    * @return information for parsing location
    */
  override def toString: String = {
    if (storage.length > 0) {
      val res = Math.min(loc, storage.length - 1)
      storage.substring(res, res + 1)
    }
    else ""
  }

  /** Provides numbers of characters that have been consumed(parsed)
    *
    * @return numbers of characters that have been consumed
    */
  override def getLoc: Int = loc
}

/** Provides implicit conversion from {{{String}}} and {{{Char}}} to {{{Location}}}
  * So that {{{"abc".parse("abc")}}} can be used directly instead of
  * {{{string("abc").parse("abc")}}}
  */
object Location {
  /** implicit conversion from {{{String}}} to {{{Location}}}
    *
    * @param s string to be converted
    * @return a Location object
    */
  implicit def fromString(s: String): Location = StringLocation(s, 0)

  /** implicit conversion from {{{Char}}} to {{{Location}}}
    *
    * @param c char to be converted
    * @return a Location object
    */
  implicit def fromChar(c: Char): Location = fromString(c.toString)
}

/** Trait for parse results. A result is either successful or not.
  *
  * @tparam A type of the input
  */
private[parsing] sealed trait ParseState[+A] {
  /** removeAttempt(true) will set the parse state to committed state.
    * If in committed state, the failure of the left branch of {{{orElse}}} will
    * cause the parser to fail directly without trying the right branch.
    *
    * @param attempted records whether the parser is committed or not
    * @return
    */
  def removeAttempt(attempted: Boolean): ParseState[A] = this match {
    case ParseFailure(m, e, c) => ParseFailure(m, e, c || attempted)
    case r => r
  }

  /** On success, advance the parser.
    * This method is mainly used by a tail recursion version of method {{{repeat}}}
    *
    * @param n the distance to be advanced.
    * @return On success, a new ParseState object.
    */
  def pushSuccess(n: Int): ParseState[A] = this match {
    case ParseSuccess(a, m) => ParseSuccess(a, m + n)
    case r => r
  }

  /** Returns whether parse is success or not
    *
    * @return whether parse is success or not
    */
  def isFailure: Boolean
}

/** The success case of `ParseState`: contains the result and the "consumed" amount.
  *
  * @param result The parsing result
  * @param consumed The amount that has been successfully parsed
  * @tparam A type of the input
  */
case class ParseSuccess[+A](result: A, consumed: Int) extends ParseState[A] {
  override def isFailure: Boolean = false
}

/** The fail case of `ParseState`: contains error message, fail location and
  * committed state.
  *
  * @param msg error message
  * @param location fail location
  * @param attempted committed state
  */
case class ParseFailure(msg: String = "", location: Location, attempted: Boolean) extends ParseState[Nothing] {
  override def toString: String = msg

  override def isFailure: Boolean = true
}

/** Provides essential combinators for char sequence based parsing.
  * The combinators are: {{{orElse}}} and its alias {{{|}}}
  * {{{andThen}}} and its alias {{{~}}}
  * {{{map}}} and {{{flatMap}}}.
  *
  * @tparam A Type of input
  */
trait Parser[+A] {
  /** An unspecified method that defines the behaviour of this parser
    *
    * @param loc Parsing location
    * @return Current parse state
    */
  protected def apply(loc: Location): ParseState[A]

  /** Do parsing and return parse result.
    *
    * @param loc Parsing location
    * @return On success, returns parse result. On failure, returns an
    *         Failure object with the error message.
    */
  def parse(loc: Location): Try[A] = {
    apply(loc) match {
      case ParseSuccess(a, _) => Success(a)
      case f@ParseFailure(_, _, _) => Failure(new Exception(f.toString))
    }
  }

  /** A parser combinator for alternative composition. Its alias is {{{|}}}
    * `p orElse q` succeeds if p succeeds or q succeeds.
    * In uncommitted state, `q` will be tried if `p` fails.
    * In committed state, failure of `p` will fail the whole
    * composition.
    *
    * @param p The "right branch" of the alternative composition.
    * @tparam B Input type of the right branch. B is defined as B >: A
    *           for variance reasons.
    * @return Alternative composition of the two parsers
    */
  def orElse[B >: A](p: => Parser[B]): Parser[B] = {
    loc =>
      apply(loc) match {
        case ParseFailure(_, _, false) => p.apply(loc)
        case r => r
      }
  }

  /** A parser combinator for sequential composition.
    * `p andThen q` succeeds if `p` succeeds and `q` succeeds
    * on the input left over by `p`.
    *
    * @param p The parser that will be executed after the current parser
    * @tparam B Type of the second parser
    * @return A tupled combination that contains the result of the two parsers.
    *         The result fails if either of the two parsers fails.
    */
  def andThen[B](p: => Parser[B]): Parser[(A, B)] =
    for {
      a <- this
      b <- p
    } yield (a, b)


  /** A shorthand alias of method {{{andThen}}}
    *
    * @param p The parser that will be executed after the current parser
    * @tparam B Type of the second parser
    * @return A tupled combination that contains the result of the two parsers.
    *         The result fails if either of the two parsers fails.
    */
  def ~[B](p: => Parser[B]): Parser[(A, B)] = this andThen p

  /** A shorthand alias of method {{{orElse}}}
    *
    * @param p The "right branch" of the alternative composition.
    * @tparam B Input type of the right branch.
    * @return Alternative composition of the two parsers
    */
  def |[B >: A](p: => Parser[B]): Parser[B] = this orElse p

  /** A parser combinator that does sequential parsing and only
    * keeps the right result and discards the left result.
    *
    * @param p The right parser
    * @tparam B Type of input of the right parser
    * @return On success, returns parsing result of the right parser
    */
  def ~>[B](p: => Parser[B]): Parser[B] = (this ~ p) map (_._2)

  /** A parser combinator that does sequential parsing and only
    * keeps the left result and discards the right result.
    * @param p The right parser
    * @tparam B Type of input of the right parser
    * @return On success, returns parsing result of the left parser
    */
  def <~[B](p: =>Parser[B]): Parser[A] = (this ~ p) map (_._1)

  /** Given a parser pa: Parser[A] and a function f: A => B,
    * the parser pa.map(f) applies pa to parse the input sequence
    * producing a: A as an intermediate result value if successful,
    * and then computes f(a): B as the final result value of the parse.
    *
    * @param f The function to be applied
    * @tparam B return type of function {{{f}}}
    * @return a parser of type B
    */
  def map[B](f: A => B): Parser[B] = {
    loc =>
      apply(loc) match {
        case ParseSuccess(a, k) => ParseSuccess(f(a), k)
        case e @ ParseFailure(_, _, _) => e
      }
  }

  /** Given a parser pa: Parser[A] and a function f: A => Parser[B],
    * the parser pa.flatMap(f) first applies pa to the input sequence
    * producing a partial parse with intermediate result a: A if successful.
    * Using a it then constructs the parser f(a): Parser[B] and uses this
    * sub-parser to parse the remaining input. f(a) then produces the final result
    * of type B. If pa or f(a) fail then so does pa.flatMap(f).
    *
    * @param f the function to be applied
    * @tparam B The result type after applying function {{{f}}}
    * @return a parser of type B
    */
  def flatMap[B](f: A => Parser[B]): Parser[B] = {
    loc =>
      apply(loc) match {
        case ParseSuccess(a, k) => f(a)(loc.advance(k)).removeAttempt(k != 0).pushSuccess(k)
        case e @ ParseFailure(_, _, _) => e
      }
  }

}

/** Defines various utility method for parsing.
  * Also provides implicits for converting from String, Char and regex to parsers.
  *
  */
object Parser {
  /** Given a parser pa: Parser[A], the parser repeat(a) applies pa as many times
    * as needed to parse the input sequence, producing the list of result values of
    * the sub-parses if successful.
    * This combinator simulates the behavior of Kleene star `*`; the repetition times
    * can be from 0 to as many as needed.
    *
    * The implementation uses tail recursion to avoid stack overflow.
    *
    * @param p the parser to be repeated
    * @tparam A Type of input
    * @return A parser that contains a list of results produced by
    *         repeatedly applying `p` to the input.
    */
  def repeat[A](p: Parser[A]): Parser[List[A]] = {
    loc =>
    @tailrec def iter(consumed: Int, acc: List[A]): ParseState[List[A]] = {
      p.apply(loc.advance(consumed)) match {
        case ParseSuccess(a, c) => iter(c + consumed, acc ::: List(a))
        case ParseFailure(_, _, _) => ParseSuccess(acc, consumed)
      }
    }
    iter(0, Nil)
  }

  /** A parser combinator for a specified number of repetitions.
    *
    * @param n repeat times
    * @param p the parser to be repeated
    * @tparam A input type
    * @return A parser that returns a list of results produced by repeatedly applying
    *         n times of `p` to the input. It only succeeds when all the n times of applying
    *         of `p` succeeds.
    */
  def repeatN[A](n: Int)(p: Parser[A]): Parser[List[A]] =
    if (n <= 0) _ => ParseSuccess(List(), 0)
    else (p andThen repeatN(n - 1)(p)) map { case (hd, tl) => hd :: tl }


  /** Change a committed parser to uncommitted state. Do nothing if the parser is already in
    * a uncommitted state.
    *
    * @param p the parser to be changed.
    * @tparam A the input type
    * @return the changed parser if applicable.
    */
  def attempt[A](p: Parser[A]): Parser[A] = {
    loc =>
      p.apply(loc) match {
        case ParseFailure(m, t, true) => ParseFailure(m, t, false)
        case r => r
      }
  }

  /** Parser label(msg)(p) behaves like p.
    * However, if p fails, then the error message contained in the error state
    * produced by p should be replaced by msg.
    *
    * This method is mainly for error handling.
    *
    * @param msg Message to be displayed
    * @param p The parser
    * @tparam A The input type
    * @return A parser with error message replaced.
    */
  def label[A](msg: String)(p: Parser[A]): Parser[A] = {
    loc =>
      p.apply(loc) match {
        case ParseFailure(_, curLoc, c) => ParseFailure(msg, curLoc, c)
        case r => r
      }
  }

  /** The parser tag(msg)(p) behaves like p.
    * However, if p fails, then the error message in p's final error state
    * should be extended with msg
    *
    * This method is mainly for error handling
    *
    * @param msg The message that extends the error message
    * @param p the parser
    * @tparam A the input type
    * @return A parser with error message extended
    */
  def tag[A](msg: String)(p: Parser[A]): Parser[A] = {
    loc =>
      p.apply(loc) match {
        case ParseFailure(m, curLoc, c) => ParseFailure(m + msg, curLoc, c)
        case r => r
      }
  }

  /** This method is almost the same with method tag, except with more informative message
    * for error handling
    *
    * @param p The parser
    * @tparam A the input type
    * @return A parser with error message extended.
    */
  def tagError[A](p: Parser[A]): Parser[A] = {
    loc =>
      p.apply(loc) match {
        case ParseFailure(msg, curLoc, c) => ParseFailure("Error (" + curLoc.getLoc + "): Found '" + curLoc + "' but " + msg, curLoc, c)
        case r => r
      }
  }

  /** "Lift" a character to a parser object by implicit conversion.
    * For example, {{{'{' ~> 'a' <~ '}'}}} can be written directly instead of
    * wrapping everything by {{{char}}}.
    * Note that since {{{|}}} is a member method of type {{{Char}}}, if alternative
    * combination operator is needed, only {{{orElse}}} is applicable because {{{|}}}
    * will NOT be treated as alias of {{{orElse}}}
    * For example, {{{'a' | 'b'}}} yields an integer value, while {{{'a' orElse 'b'}}}
    * is the correct expression.
    *
    * The parser simply matches the char in parser with the char to be parsed.
    * Parse succeeds if char matches; otherwise fails.
    * For example: {{{'a'.parse('a')}}} succeeds while {{{'a'.parse('b')}}} fails
    *
    * @param c the character to be lifted
    * @return an object of Parser[Char] that can parse the character
    */
  implicit def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))

  /** "Lift" a string to a parser object by implicit conversion.
    * For example, {{{"begin" ~> "string" <~ "end"}}} can be written directly instead of
    * wrapping everything by {{{string}}}.
    *
    * @param s the string to be lifted
    * @return an object of Parser[String] that can parse the string
    */
  implicit def string(s: String): Parser[String] = {
    case StringLocation(cur, start) =>
      val len = if (s.length < cur.length - start) s.length else cur.length - start
      if (s == cur.substring(start, start + len)) ParseSuccess(s, s.length)
      else ParseFailure("", StringLocation(cur, start), false)
  }

  /** "Lift" a string to a parser object by implicit conversion.
    * For example {{{"\\s*".r ~> "\\d+".r <~"\\s*".r}}} can be written directly instead of
    * wrapping everything by {{{regex}}}.
    *
    * @param r the regex
    * @return an object of Parser[String] that can parse the regex
    */
  implicit def regex(r: Regex): Parser[String] = {
    case StringLocation(curStr, start) =>
      r.findFirstMatchIn(curStr.substring(start)) match {
        case Some(s) if s.start == 0 =>
          ParseSuccess(s.matched, s.matched.length)
        case _ => ParseFailure("", StringLocation(curStr, start), false)
      }
  }

  def digit: Parser[Int] = "[0-9]".r map (_.toInt)
  // def digit: Parser[Int] = ('0' to '9') reduce (_ orElse _) map (_.toString.toInt)

  def digits: Parser[Int] =
    (digit andThen repeat(digit)) map { case (d, ds) => (d :: ds) reduce (_ * 10 + _) }

  /** This is used to mark the end of the string to be parsed
    *
    * @return A parser that parses the end of the string
    */
  def eof: Parser[String] = "\\z".r

  /** This is used to handle(parse) the spaces in the string
    *
    * @return A parser that parses spaces
    */
  def spaces: Parser[String] = "\\s*".r

  /** This is used to handle(parse) double numbers
    *
    * @return A parser that parses double numbers
    */
  def number: Parser[Double] = "(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?([Ee]([+-]?)(0|([1-9][0-9]*)))?".r map (_.toDouble)

  /** The following are auxiliary methods that is used to skip part of the input.
    *
    * @param skip The parser for input that is to be skipped
    * @param p The parser for input that is to keep.
    * @tparam A Type of input that is to keep.
    * @tparam B Type of input that is to be skipped.
    * @return A parser that is for the input that keeps.
    */
  def trim[A, B](skip: Parser[B])(p: Parser[A]): Parser[A] = skip ~> p <~ skip

  def trimLeft[A, B](skip: Parser[B])(p: Parser[A]): Parser[A] = skip ~> p

  def trimRight[A, B](skip: Parser[B])(p: Parser[A]): Parser[A] = p <~ skip

  def trimSpaces[A](p: Parser[A]): Parser[A] = trim(spaces)(p)

  def enclose[A, B](left: Parser[B], right: Parser[B])(p: Parser[A]): Parser[A] = left ~> p <~ right

  /** 
    *
    * @param p
    * @param b
    * @tparam A
    * @tparam B
    * @return
    */
  def into[A, B](p: Parser[A], b: B): Parser[B] = p map (_ => b)

  def list[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] = {
    (for {
      a <- p
      as <- repeat(sep ~> p)
    } yield a :: as) | (_ => ParseSuccess(List[A](), 0))
  }

  def mapFailure[A](msg: String)(p: Parser[A]): Parser[A] = {
    loc =>
      p.apply(loc) match {
        case f @ ParseFailure(m, loc, c) => {
          if (m == "") ParseFailure(msg, loc, c) else f
        }
        case r => r
      }
  }

}


