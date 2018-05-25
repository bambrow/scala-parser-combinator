package calculator

import scala.language.postfixOps
import org.scalatest.FlatSpec
import Calculator._

class CalculatorSpec extends FlatSpec {

  import parsing.Parser._

  "expressionParser" should "parse single number" in {
    assert(Calculator.parse("1") === 1.0)
    assert(Calculator.parse("-1") === -1.0)
    assert(Calculator.parse(" 1") === 1.0)
    assert(Calculator.parse("1 ") === 1.0)
    assert(Calculator.parse(" 1 ") === 1.0)
    assert(Calculator.parse(" - 1 ") === -1.0)
    assert(Calculator.parse(" - -1 ") === 1.0)
  }

  "expressionParser" should "parse simple binary expression" in {
    assert(Calculator.parse("1+2") === 3.0)
    assert(Calculator.parse("1 + 2") === 3.0)
    assert(Calculator.parse(" 1 + 2 ") === 3.0)
    assert(Calculator.parse("1 + -2") === -1.0)
    assert(Calculator.parse(" 1  +  -2 ") === -1.0)
    assert(Calculator.parse("1-2") === -1.0)
    assert(Calculator.parse("1 - 2") === -1.0)
    assert(Calculator.parse(" 1 - 2 ") === -1.0)
    assert(Calculator.parse("1 - -2") === 3.0)
    assert(Calculator.parse(" 1  -  -2 ") === 3.0)
    assert(Calculator.parse("2*3") === 6.0)
    assert(Calculator.parse("2 * 3") === 6.0)
    assert(Calculator.parse(" 2 * 3 ") === 6.0)
    assert(Calculator.parse("2 * -3") === -6.0)
    assert(Calculator.parse(" 2  *  -3 ") === -6.0)
    assert(Calculator.parse("1/2") === .5)
    assert(Calculator.parse("1 / 2") === .5)
    assert(Calculator.parse(" 1 / 2 ") === .5)
    assert(Calculator.parse("1 / -2") === -.5)
    assert(Calculator.parse(" 1  /  -2 ") === -.5)
    assert(Calculator.parse(" 1  + - -2 ") === 3.0)
    assert(Calculator.parse(" 1  - - -2 ") === -1.0)
    assert(Calculator.parse(" 2  * - -3 ") === 6.0)
    assert(Calculator.parse(" 1 / - -2 ") === .5)
  }

  "expressionParser" should "parse simple enclosed binary expression" in {
    assert(Calculator.parse("(1+2)") === 3.0)
    assert(Calculator.parse("(1 + 2)") === 3.0)
    assert(Calculator.parse(" ( 1 + 2 ) ") === 3.0)
    assert(Calculator.parse("(1 + -2)") === -1.0)
    assert(Calculator.parse(" ( 1  +  -2 ) ") === -1.0)
    assert(Calculator.parse("(1-2)") === -1.0)
    assert(Calculator.parse("(1 - 2)") === -1.0)
    assert(Calculator.parse(" ( 1 - 2 ) ") === -1.0)
    assert(Calculator.parse("(1 - -2)") === 3.0)
    assert(Calculator.parse(" ( 1  -  -2 ) ") === 3.0)
    assert(Calculator.parse("(2*3)") === 6.0)
    assert(Calculator.parse("(2 * 3)") === 6.0)
    assert(Calculator.parse(" ( 2 * 3 ) ") === 6.0)
    assert(Calculator.parse("(2 * -3)") === -6.0)
    assert(Calculator.parse(" ( 2  *  -3 ) ") === -6.0)
    assert(Calculator.parse("(1/2)") === .5)
    assert(Calculator.parse("(1 / 2)") === .5)
    assert(Calculator.parse(" ( 1 / 2 ) ") === .5)
    assert(Calculator.parse("(1 / -2)") === -.5)
    assert(Calculator.parse(" ( 1  /  -2 ) ") === -.5)
    assert(Calculator.parse(" - (1  + - -2) ") === -3.0)
    assert(Calculator.parse(" - (1  - - -2) ") === 1.0)
    assert(Calculator.parse(" - (2  * - -3) ") === -6.0)
    assert(Calculator.parse(" - (1 / - -2) ") === -.5)
  }

  "expressionParser" should "parse more enclosed expression" in {
    assert(Calculator.parse(" (2 + 3) * (4 + 5) ") === 45.0)
    assert(Calculator.parse(" (2 + 3) * 4 ") === 20.0)
    assert(Calculator.parse(" 3 * (4 + 5) ") === 27.0)
    assert(Calculator.parse(" -(2 + 3) * -(4 + 5) ") === 45.0)
    assert(Calculator.parse(" -(2 + 3) * 4 ") === -20.0)
    assert(Calculator.parse(" 3 * -(4 + 5) ") === -27.0)
  }

  "expressionParser" should "parse sequential expression with precedence" in {
    assert(Calculator.parse(" 1 + 2 + 3 ") === 6.0)
    assert(Calculator.parse(" 1 + 2 * 3 ") === 7.0)
    assert(Calculator.parse(" 1 + 2 / 4 ") === 1.5)
    assert(Calculator.parse(" 1 + 2 - 1 * 2 + 2 ") === 3.0)
    assert(Calculator.parse(" 1 + 2 - -1 * 2 + 2 ") === 7.0)
    assert(Calculator.parse(" 1 + 2 - - -1 * 2 + 2 ") === 3.0)
  }

  "expressionParser" should "parse multiple times inverse number and expression" in {
    assert(Calculator.parse(" --1 ") === 1.0)
    assert(Calculator.parse(" ---1 ") === -1.0)
    assert(Calculator.parse(" ----1 ") === 1.0)
    assert(Calculator.parse(" - - -1 ") === -1.0)
    assert(Calculator.parse(" -- -- -- --1 ") === 1.0)
    assert(Calculator.parse(" - -- - --(1 + 1) ") === 2.0)
    assert(Calculator.parse(" --1 + --1 ") === 2.0)
    assert(Calculator.parse(" -- 1 + -- 1 ") === 2.0)
  }

  "expressionParser" should "parse enclosed number" in {
    assert(Calculator.parse(" ( 1 ) ") === 1.0)
    assert(Calculator.parse(" ( -1 ) ") === -1.0)
    assert(Calculator.parse(" ( - 1 ) ") === -1.0)
    assert(Calculator.parse(" - ( 1 ) ") === -1.0)
    assert(Calculator.parse(" - ( -1 ) ") === 1.0)
    assert(Calculator.parse(" - ( - 1 ) ") === 1.0)
    assert(Calculator.parse(" 2 * ( 1 ) ") === 2.0)
    assert(Calculator.parse(" 2 * ( -1 ) ") === -2.0)
    assert(Calculator.parse(" 2 * ( - 1 ) ") === -2.0)
    assert(Calculator.parse(" 2 * - ( 1 ) ") === -2.0)
    assert(Calculator.parse(" 2 * - ( -1 ) ") === 2.0)
    assert(Calculator.parse(" 2 * - ( - 1 ) ") === 2.0)
  }

  "expressionParser" should "parse multiple enclosed expression" in {
    assert(Calculator.parse(" ((2 + 3) * (4 + 5)) ") === 45.0)
    assert(Calculator.parse(" (((2 + 3)) * 4) ") === 20.0)
    assert(Calculator.parse(" ((((3 * (4 + 5))))) ") === 27.0)
    assert(Calculator.parse(" -(-(-(2 + 3))) * -((4) + (((5)))) ") === 45.0)
    assert(Calculator.parse(" (-(2 + ((3)))) * (((((4))))) ") === -20.0)
    assert(Calculator.parse(" ((((-(-(3))) * (-(4 + 5))))) ") === -27.0)
  }

  "expressionParser" should "handle corner cases" in {
    assert(Calculator.parse(" -0 + -0 ") === 0.0)
    assert(Calculator.parse(" -0 - -0 ") === 0.0)
    assert(Calculator.parse(" -0 * -0 ") === 0.0)
    assert(Calculator.parse(" -1 / 0 ").isNegInfinity)
    assert(Calculator.parse(" -1 / -0 ").isNegInfinity)
    assert(Calculator.parse(" 1 / 0 ").isPosInfinity)
    assert(Calculator.parse(" 1 / -0 ").isPosInfinity)
    assert(Calculator.parse(" 0 / 0 ").isNaN)
    assert(Calculator.parse(" -0 / 0 ").isNaN)
    assert(Calculator.parse(" 0 / -0 ").isNaN)
    assert(Calculator.parse(" -0 / -0 ").isNaN)
  }

  "expressionParser" should "throw error with invalid input" in {
    intercept[Exception] { Calculator parse "" }
    intercept[Exception] { Calculator parse "()" }
    intercept[Exception] { Calculator parse "(1" }
    intercept[Exception] { Calculator parse "1)" }
    intercept[Exception] { Calculator parse "2 ++ 1" }
    intercept[Exception] { Calculator parse "2 +" }
    intercept[Exception] { Calculator parse "2 */ 1" }
    intercept[Exception] { Calculator parse "x + y" }
    intercept[Exception] { Calculator parse "(((2 + 3))" }
  }


}
