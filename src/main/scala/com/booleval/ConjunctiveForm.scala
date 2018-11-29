package com.booleval


object ConjunctiveForm {

  def convert(exp: BooleanExpression): BooleanExpression = exp match {
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

  def isInForm(exp: BooleanExpression): Boolean = exp match {
    case Or(l, r) => matchOrs(l) && matchOrs(r)
    case And(_, _) => matchAnds(exp)
    case _ => true
  }

  private def crossFixingLhs(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = rhs match {
    case And(l, r) => And(crossFixingLhs(lhs, l), crossFixingLhs(lhs, r))
    case True | False | Variable(_) | Not(_) => Or(lhs, rhs)
    case Or(_, _) => Or(lhs, rhs)
  }

  private def crossFixingRhs(lhs: BooleanExpression, rhs: BooleanExpression): BooleanExpression = lhs match {
    case Or(_, _) => crossFixingLhs(lhs, rhs)
    case And(l, r) => And(crossFixingRhs(l, rhs), crossFixingRhs(r, rhs))
    case True | False | Variable(_) | Not(_) => crossFixingLhs(lhs, rhs)
  }

  private def matchOrs(exp: BooleanExpression): Boolean = exp match {
    case Or(l, r) => matchOrs(l) && matchOrs(r)
    case And(_, _) => false
    case _ => true
  }

  private def matchAnds(exp: BooleanExpression): Boolean = exp match {
    case And(l, r) => (l, r) match {
      case (And(_, _), And(_, _)) => matchAnds(l) && matchAnds(r)
      case (And(_, _), _) => matchAnds(l) && matchOrs(r)
      case (_, And(_, _)) => matchOrs(l) && matchAnds(r)
      case (_, _) => matchOrs(l) && matchOrs(r)
    }
    case _ => false
  }

}
