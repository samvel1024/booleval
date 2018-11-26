package com.booleval

sealed trait BooleanExpression {

  type Evaluation = String => Option[BooleanConstant]

  def evaluate(v: Evaluation): BooleanConstant = this match {
    case True => True
    case False => False
    case Variable(symbol) => v.apply(symbol) match {
      case Some(value) => value
      case None => throw new RuntimeException(s"No variable value provided for $symbol")
    }
    case Not(e) => e.evaluate(v) match {
      case True => False
      case False => True
    }
    case Or(a, b) => (a.evaluate(v), b.evaluate(v)) match {
      case (False, False) => False
      case _ => True
    }
    case And(a, b) => (a.evaluate(v) , b.evaluate(v)) match {
      case (True, True) => True
      case _ => False
    }
  }

  override def toString: String = this match {
    case False => "0"
    case True => "1"
    case Or(l, r) => s"(${l toString()} | ${r toString()})"
    case And(l, r) => s"(${l toString()} & ${r toString()})"
    case Not(e) => s"!${e toString()}"
    case Variable(v) => v
  }

  def |(v: BooleanExpression) = Or(this, v)

  def &(v: BooleanExpression) = And(this, v)

  def unary_! = Not(this)

}

object BooleanExpressionImplicit {

  implicit class StringToVariable(s: String) {
    def unary_~(): Variable = Variable(s)
  }

}

abstract class BooleanConstant(value: Boolean) extends BooleanExpression{
  def toBoolean: Boolean = value
}

case object True extends BooleanConstant(true)

case object False extends BooleanConstant(false)

case class Variable(symbol: String) extends BooleanExpression

case class Not(exp: BooleanExpression) extends BooleanExpression

case class Or(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

case class And(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression


