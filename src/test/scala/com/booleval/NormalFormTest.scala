package com.booleval

import com.booleval.BooleanExpressionImplicit._
import org.scalatest.FunSuite

class NormalFormTest extends FunSuite {


  def assertSame(a: BooleanExpression, nf: BooleanExpression): Unit = {
    assert(a.equivalentTo(nf))
  }

  test("Or case in CNF") {

    val list: List[BooleanExpression] = List(
      ((!(~"a") | ~"b") & (~"a" | ~"b")) | (!(~"d") | !(~"a")),
      (~"a" | ~"b") & True,
      ((~"a" & !(~"b")) | ~"c") | ~"d",
      (~"a" & ~"b") | (~"c" & ~"d")
    )
    list.map(i => (i, Conjunctive.convert(i)))
      .foreach(((a: BooleanExpression, b: BooleanExpression) => assertSame(a, b)).tupled)
  }


  test("Big conjunctive form test") {
    Generator.generate(10, 4,
      Array("a", "s", "d", "f", "g", "h", "j", "k", "l", "z", "x", "c", "v"), useConstants = true)
      .foreach(exp => assertSame(exp, Conjunctive.convert(exp)))
  }


}
