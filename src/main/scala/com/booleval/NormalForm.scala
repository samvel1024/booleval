package com.booleval


sealed trait NormalForm {
  def convert(exp: BooleanExpression): BooleanExpression
}

object Conjunctive extends NormalForm {


  def crossFixingLhs(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = rhs match {
    case And(l, r) => And(crossFixingLhs(lhs, l), crossFixingLhs(lhs, r))
    case True | False | Variable(_) | Not(_) => Or(lhs, rhs)
    case Or(_, _) => throw new RuntimeException()
  }

  def crossFixingRhs(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = lhs match {
    case Or(_, _) => Or(lhs, rhs)
    case And(l, r) => And(crossFixingRhs(l, rhs), crossFixingRhs(r, rhs))
    case True | False | Variable(_) | Not(_) => crossFixingLhs(lhs, rhs)
  }

  override def convert(exp: BooleanExpression): BooleanExpression = exp match {
    case Variable(_) | True | False => exp
    case Not(negated) => negated match {
      case Or(lhs, rhs) => convert(And(Not(lhs), Not(rhs)))
      case And(lhs, rhs) => convert(Or(Not(lhs), Not(rhs)))
      case Not(p) => convert(p)
      case True => False
      case False => True
      case Variable(_) => exp
    }
    case And(lhs, rhs) => And(convert(lhs), convert(rhs))
    case Or(lhs, rhs) => crossFixingRhs(convert(lhs), convert(rhs))
  }

}
