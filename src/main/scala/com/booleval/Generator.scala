package com.booleval

import scala.util.Random

object Generator {

  def pickRandom[T](arr: Array[T]): T = {
    arr.apply(Random.nextInt(arr.length))
  }

  def pickRandomF[T](size: Int, switch: Int => T): T = {
    switch.apply(Random.nextInt(size))
  }

  def generate(remaining: Int, varNames: Array[String], useConstants: Boolean): BooleanExpression = {
    if (remaining == 0)
      return pickRandomF(if (useConstants) 3 else 1, {
        case 0 => Variable(pickRandom(varNames))
        case 1 => True
        case 2 => False
      })
    val lhs = generate(remaining - 1, varNames, useConstants)
    val rhs = generate(remaining - 1, varNames, useConstants)
    pickRandomF(2, {
      case 0 => Or
      case 1 => And
    }).apply(lhs, rhs)
  }

  def generate(count: Int, remaining: Int, varNames: Array[String], useConstants: Boolean): Iterator[BooleanExpression] = {
    Iterator.tabulate(count)(_ => generate(remaining, varNames, useConstants))
  }


}
