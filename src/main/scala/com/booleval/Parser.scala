package com.booleval

import scala.util.parsing.combinator.JavaTokenParsers

object Parser extends JavaTokenParsers {

  /**
    * Semantically the inverse function of BooleanExpression.toString
    */
  def parse(str: String): BooleanExpression =
    parseAll(expression, str) match {
      case Success(result, _) => result
      case NoSuccess(msg, _) => throw new RuntimeException(msg)
    }

  private def expression: Parser[BooleanExpression] = not | variable | literal | or | and

  private def not: Parser[BooleanExpression] = "!" ~> expression ^^ Not

  private def variable: Parser[BooleanExpression] = "[a-z]".r ^^ Variable

  private def or: Parser[BooleanExpression] = ("(" ~> expression <~ "|") ~ expression <~ ")" ^^ {
    case l ~ r => Or(l, r)
  }

  private def and: Parser[BooleanExpression] = ("(" ~> expression <~ "&") ~ expression <~ ")" ^^ {
    case l ~ r => And(l, r)
  }

  private def literal: Parser[BooleanExpression] = {
    ("1" | "0") ^^ {
      case "1" => True
      case "0" => False
    }
  }
}
