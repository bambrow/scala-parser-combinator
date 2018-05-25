package calculator

import parsing.Parser
import scala.io.StdIn

object Calculator {

  sealed abstract class Expression

  case class Number(n: Double) extends Expression
  case class Exp(operator: Operator, left: Expression, right: Expression) extends Expression

  sealed abstract class Operator

  case object Plus extends Operator
  case object Minus extends Operator
  case object Times extends Operator
  case object Div extends Operator

  def expressionParser: Parser[Expression] = {

    import parsing.Parser._

    val inverse: Parser[Char] = attempt('-')

    val num: Parser[Expression] = tag("single number")(attempt(number map (x => if (x == 0) Number(0) else Number(x))))

    val inverseNum: Parser[Expression] = tag("inverse number")(attempt(inverse ~> attempt(number)) map (x => if (x == 0) Number(0) else Exp(Times, Number(x), Number(-1))))

    lazy val enclosedExp: Parser[Expression] = tag("enclosed expression")(attempt(enclose('(', ')')(exp)))

    lazy val inverseEnclosedExp: Parser[Expression] = tag("inverse enclosed expression")(attempt(inverse ~> attempt(enclose('(', ')')(exp))) map (Exp(Times, _, Number(-1))))

    lazy val inverseExp: Parser[Expression] = tag("inverse expression")(attempt(inverse ~> exp) map (Exp(Times, _, Number(-1))))

    lazy val factor: Parser[Expression] = num | enclosedExp | inverseNum | inverseEnclosedExp | inverseExp

    lazy val term: Parser[Expression] = (factor ~ repeat(('*' orElse '/') ~ factor)) map {
      case (x, xs) => xs.foldLeft(x) {
        case (y, ('*', z)) => Exp(Times, y, z)
        case (y, ('/', z)) => Exp(Div, y, z)
      }
    }

    lazy val exp: Parser[Expression] = (term ~ repeat(('+' orElse '-') ~ term)) map {
      case (x, xs) => xs.foldLeft(x) {
        case (y, ('+', z)) => Exp(Plus, y, z)
        case (y, ('-', z)) => Exp(Minus, y, z)
      }
    }

    exp <~ label("unexpected trailing characters")(eof)

  }

  def evaluate(e: Expression): Double = e match {
    case Number(n) => n
    case Exp(operator, left, right) => {
      val l = evaluate(left)
      val r = evaluate(right)
      operator match {
        case Plus => l + r
        case Minus => l - r
        case Times => l * r
        case Div => {
          if (l > 0 && r == 0) Double.PositiveInfinity
          if (l < 0 && r == 0) Double.NegativeInfinity
          if (l == 0 && r == 0) Double.NaN
          l / r
        }
      }
    }
  }

  def parse(s: String): Double = expressionParser.map(evaluate).parse(s.replaceAll("\\s", "")).get

  def main(args: Array[String]): Unit = {
    if (args.length != 2 && args.length != 1) {
      println("Usage: You can type [run Calculator] directly to run the expression parser. In the parser, type \"exit\", \"quit\" or \"q\" to exit. \n       You can also type [run Calculator <expression>] to evaluate the expression directly.")
    }
    if (args.length == 2) {
      println(parse(args(1)))
    }
    if (args.length == 1) {
      while ({ val line = StdIn.readLine(); (line != null && line.toLowerCase != "exit" && line.toLowerCase != "quit" && line.toLowerCase != "q") && { println(parse(line)); true } }) { }
    }
  }

}
