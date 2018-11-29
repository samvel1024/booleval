package com.booleval

import com.booleval.BooleanExpressionImplicit._
import org.scalatest.FunSuite

class ConjunctiveFormTest extends FunSuite {

  val variables = Array("a", "s", "d", "f", "g", "h", "j", "k", "l", "z", "x", "c", "v")
  val smallGenerator: (Int, Int) => Iterator[BooleanExpression] = Generator.generate(useConstants = true, variables)
  val bigGenerator: (Int, Int) => Iterator[BooleanExpression] = Generator.generate(useConstants = true, variables)

  def assertConversionOK(a: BooleanExpression): Unit = {
    val nf = ConjunctiveForm.convert(a)
    assert(a.equivalentTo(nf))
    assert(ConjunctiveForm.isInForm(nf))
  }

  test("Or case in CNF") {
    val list: List[BooleanExpression] = List(
      ((!(~"a") | ~"b") & (~"a" | ~"b")) | (!(~"d") | !(~"a")),
      (~"a" | ~"b") & True,
      ((~"a" & !(~"b")) | ~"c") | ~"d",
      (~"a" & ~"b") | (~"c" & ~"d"))
    list.foreach(exp => assertConversionOK(exp))
  }

  test("Big conjunctive form test") {
    bigGenerator(5, 4)
      .foreach(exp => assertConversionOK(exp))
  }

  test("Small conjunctive form tests") {
    smallGenerator(3, 100)
      .foreach(exp => assertConversionOK(exp))
  }

  test("Previously failed 1") {
    val list: List[String] = List("((v&h)|(0|z))")
    list.map(str => Parser.parse(str))
      .foreach(exp => assertConversionOK(exp))
  }


  test("Previously failed 3") {
    val exp = (~"a" | ~"b") & True
    assertConversionOK(exp)
  }

  test("Previously failed 4") {
    assertConversionOK((~"a" | ~"b") | (~"b" & ~"c"))
  }

  test("Previously failed 5") {
    assertConversionOK((~"b" & ~"c") | (~"a" | ~"b"))
  }

}
