package com.booleval

import com.booleval.BooleanExpressionImplicit._
import org.scalatest.FunSuite

class NormalFormTest extends FunSuite {


  test("Or case in CNF") {

    val list: List[BooleanExpression] = List(
      (~"a" | ~"b") & True,
      ((~"a" & ~"b") | ~"c") | ~"d",
      (~"a" & ~"b") | (~"c" & ~"d")
    )


    list.map(i => (i, Conjunctive.convert(i)))
      .foreach(p => {
        println(p._1.prettyPrint())
        println(p._2.prettyPrint())
        println()
      })
  }

}