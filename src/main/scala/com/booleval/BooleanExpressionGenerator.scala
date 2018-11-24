package com.booleval

import scala.util.Random

object BooleanExpressionGenerator {

  def pickRandom[T](arr: Array[T]): T = {
    arr.apply(Random.nextInt(arr.length))
  }

  def pickRandomF[T](size: Int, switch: Int => T) = {
    switch.apply(Random.nextInt(size))
  }

  def generate(remaining: Int, varNames: Array[String]): BooleanExpression = {
    if (remaining == 0)
      return pickRandomF(3, {
        case 0 => Variable(pickRandom(varNames))
        case 1 => True
        case 2 => False
      })
    val lhs = generate(remaining - 1, varNames)
    val rhs = generate(remaining - 1, varNames)
    pickRandomF(2, {
      case 0 => Or
      case 1 => And
    }).apply(lhs, rhs)
  }

}