# Parser Combinator Project

The goal of this project is to implement a parser combinator
library.

## Project Contributor

- Liheng Gong
- Weiqiang Li

## Parser Combinator & JSON Parser

Our parser combinator is able to accept a `String` input and parse
from this input using the parser specified by user. With error 
reporting mechanism, our parser combinator will let user to specify, label and
tag error messages when there is any error.

Using our library, a JSON parser is implemented to support parsing
of a JSON `String` input. See [here](https://www.json.org/) for a description of the
JSON syntax. Our JSON parser will converts the JSON `String` input into an abstract
syntax tree representing the specified data. Similarly, our JSON parser
will report error when the input `String` is not considered as a valid
JSON input.

Our parser combinator and JSON parser are both well-documented with extensive
unit tests.

## Command Line Calculator

A simple command line calculator is implemented using our parser combinator,
  which parses arithmetic
  expressions over floating point numbers and evaluates them. The
  calculator is implemented as a `Parser[Double]` that
  evaluates the expression on-the-fly during parsing.

This command line calculator can be run in sbt shell. Type `run Calculator` directly
in sbt shell will start the calculator that accepts the input from user, and calculate
the result on-the-fly. In this mode, type `quit`, `q` or `exit` to terminate
the calculator program.
Type `run Calculator <expression>` directly in sbt shell
will start the calculator and evaluates the `<expression>` user specified
in command line input, and directly give the result.
