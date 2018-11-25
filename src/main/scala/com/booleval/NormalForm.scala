package com.booleval


trait NormalForm {
  def convert(exp:BooleanExpression) : BooleanExpression
}

object Conjunctive extends NormalForm {


  def cross(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = rhs match {
    case And(nlhs, nrhs) => And(cross(lhs, nlhs), cross(lhs, nrhs))
    case True | False | Variable(_) => Or(lhs, rhs)
  }

  def lhsCrossRhs(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = lhs match {
    case And(nlhs, nrhs) => And(lhsCrossRhs(nlhs, rhs), lhsCrossRhs(nrhs, rhs))
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
      case Variable(_) => negated
    }
    case And(lhs, rhs) => And(convert(lhs), convert(rhs))
    case Or(lhs, rhs) => lhsCrossRhs(convert(lhs), convert(rhs))
  }

}
