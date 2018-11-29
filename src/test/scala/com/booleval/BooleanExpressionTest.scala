package com.booleval

import com.booleval.BooleanExpressionImplicit._
import org.scalatest.FunSuite

class BooleanExpressionTest extends FunSuite {

  test("Test simple evaluation") {
    val exp: BooleanExpression = !(~"a") | ~"b" & ~"c"
    val eval: Map[String, BooleanConstant] = Map("a" -> False, "b" -> True, "c" -> True)
    assert(exp.evaluate(eval.get).toBoolean)
  }

  test("Missing mapping throws exception") {
    val exp: BooleanExpression = ~"a" & ~"b" | ~"c"
    val eval: Map[String, BooleanConstant] = Map("a" -> False, "c" -> True)
    assertThrows[RuntimeException] {
      exp.evaluate(eval.get)
    }
  }

  test("Previously failed 1") {
    ((~"a" | (~"c" | ~"d")) & (~"b" | (~"c" | ~"d"))).equivalentTo(
      (~"a" & ~"b") | (~"c" | ~"d"))
  }

}
