package com.booleval


trait NormalForm {
  def convert(exp: BooleanExpression): BooleanExpression
}

object Conjunctive extends NormalForm {


  def cross(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = rhs match {
    case And(l, r) => And(cross(lhs, l), cross(lhs, r))
    case True | False | Variable(_) |  Not(_) => Or(lhs, rhs)
  }

  def lhsCrossRhs(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = lhs match {
    case Or(_,_) => Or(lhs, rhs)
    case And(l, r) => And(lhsCrossRhs(l, rhs), lhsCrossRhs(r, rhs))
    case True | False | Variable(_) | Not(_) => cross(lhs, rhs)
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
    case Or(lhs, rhs) => lhsCrossRhs(convert(lhs), convert(rhs))
  }

}
