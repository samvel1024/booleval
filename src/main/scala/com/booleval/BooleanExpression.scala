package com.booleval

sealed trait BooleanExpression {

  type Evaluation = String => Option[BooleanConstant]
  type VarSpace = Set[String]
  type TruthTable = Set[VarSpace]

  /**
    * @param v values of variables
    * @return True or False
    * @throws RuntimeException if there exists a variable without value mapping in v
    */
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
    case And(a, b) => (a.evaluate(v), b.evaluate(v)) match {
      case (True, True) => True
      case _ => False
    }
  }

  override def toString: String = this match {
    case False => "0"
    case True => "1"
    case Or(l, r) => s"(${l toString()}|${r toString()})"
    case And(l, r) => s"(${l toString()}&${r toString()})"
    case Not(e) => s"!${e toString()}"
    case Variable(v) => v
  }

  def truthTable(): TruthTable = truthTable(collectVarSpace(Set()))

  def equivalentTo(exp: BooleanExpression): Boolean = {
    val unionVarSpace = exp.collectVarSpace(Set()) ++ this.collectVarSpace(Set())
    exp.truthTable(unionVarSpace) == this.truthTable(unionVarSpace)
  }

  def |(v: BooleanExpression) = Or(this, v)

  def &(v: BooleanExpression) = And(this, v)

  def unary_! = Not(this)

  /**
    * @return Set of all variables in this expression
    */
  private def collectVarSpace(set: VarSpace): VarSpace = this match {
    case Variable(name) => set + name
    case And(l, r) => r.collectVarSpace(l.collectVarSpace(set))
    case Or(l, r) => r.collectVarSpace(l.collectVarSpace(set))
    case Not(e) => e.collectVarSpace(set)
    case _ => set
  }

  /**
    * @param variables - has to be a super set of the set returned from collectVarSpace
    * @return All sets of variables which (by having value True) will satisfy the expression
    */
  private def truthTable(variables: VarSpace): TruthTable = {
    val trueVars = variables.subsets()
    trueVars
      .filter(set => evaluate(variable => if (set.contains(variable)) Option(True) else Option(False)).toBoolean)
      .toSet
  }

}

object BooleanExpressionImplicit {

  implicit class StringToVariable(s: String) {
    def unary_~(): Variable = Variable(s)
  }

}

sealed trait BooleanConstant {
  def toBoolean: Boolean
}

case object True extends BooleanExpression with BooleanConstant {
  override def toBoolean: Boolean = true
}

case object False extends BooleanExpression with BooleanConstant {
  override def toBoolean: Boolean = false
}

case class Variable(symbol: String) extends BooleanExpression

case class Not(exp: BooleanExpression) extends BooleanExpression

case class Or(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression

case class And(lhs: BooleanExpression, rhs: BooleanExpression) extends BooleanExpression


