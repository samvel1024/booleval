package com.booleval

import scala.util.Random

object Generator {

  def pickRandom[T](arr: Array[T]): T = {
    arr.apply(Random.nextInt(arr.length))
  }

  def pickRandomF[T](size: Int, switch: Int => T): T = {
    switch.apply(Random.nextInt(size))
  }

  /**
    * @param height       Height of the expression tree
    * @param varNames     Names of variables that will be used
    * @param useConstants If constants can appear in leaves
    * @return A newly generated tree with given height
    */
  def generate(height: Int, varNames: Array[String], useConstants: Boolean): BooleanExpression = {
    if (height == 0)
      return pickRandomF(if (useConstants) 3 else 1, {
        case 0 => Variable(pickRandom(varNames))
        case 1 => True
        case 2 => False
      })
    val lhs = generate(height - 1, varNames, useConstants)
    val rhs = generate(height - 1, varNames, useConstants)
    pickRandomF(2, {
      case 0 => Or
      case 1 => And
    }).apply(lhs, rhs)
  }

  def generate(useConstants: Boolean, varNames: Array[String])(height: Int, count: Int): Iterator[BooleanExpression] = {
    Iterator.tabulate(count)(_ => generate(height, varNames, useConstants))
  }


}
