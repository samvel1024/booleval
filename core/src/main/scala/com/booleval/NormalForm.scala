package com.booleval

object NormalForm {

//  def cnf(exp: BooleanExpression) : BooleanExpression = exp match {
//    case Variable(_) | True | False => exp
//    case Not(negated) => negated match {
//      case Or(lhs, rhs) => cnf(And(Not(lhs), Not(rhs)))
//      case And(lhs, rhs) => cnf(Or(Not(lhs), Not(rhs)))
//      case Not(p) => cnf(p)
//      case True => False
//      case False => True
//      case Variable(s) => Variable(s)
//    }
//    case And(lhs, rhs) => And(cnf(lhs), cnf(rhs))
//    case Or(lhs, rhs) =>
//  }

}
