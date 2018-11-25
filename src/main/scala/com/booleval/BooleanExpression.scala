package com.booleval

sealed trait BooleanExpression {

  type Evaluation = String => Boolean

  def evaluate(v: Evaluation): Boolean = this match {
    case True => true
    case False => false
    case Variable(symbol) => v.apply(symbol);
    case Not(e) => !e.evaluate(v);
    case Or(a, b) => a.evaluate(v) || b.evaluate(v);
    case And(a, b) => a.evaluate(v) && b.evaluate(v);
  }

  def prettyPrint(): String = this match {
    case False => "0"
    case True => "1"
    case Or(l, r) => s"(${l prettyPrint()} | ${r prettyPrint()})"
    case And(l, r) => s"(${l prettyPrint()} & ${r prettyPrint()})"
    case Not(e) => s"!${e prettyPrint()}"
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

case object True extends BooleanExpression

case object False extends BooleanExpression

case class Variable(symbol: String) extends BooleanExpression

case class Not(exp: BooleanExpression) extends BooleanExpression

case class Or(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

case class And(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression


