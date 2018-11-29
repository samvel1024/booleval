package com.booleval

import com.booleval.BooleanExpressionImplicit._
import org.scalatest.{Assertion, FunSuite}
import play.api.libs.json.Json

class SerializationTest extends FunSuite {

  type Serialize = BooleanExpression => String
  type Deserialize = String => BooleanExpression

  val jsonAssert: BooleanExpression => Unit = parsingAssertion(
    exp => JsonSerial.serialize(exp).toString(),
    str => JsonSerial.deserialize(Json.parse(str)))

  val lexerAssert: BooleanExpression => Unit = parsingAssertion(
    e => e.toString,
    s => Parser.parse(s))

  val smallCases = List(!(~"a"), !True, !False, False, True, ~"b", True | False, ~"a" & !(~"b"), False & !True)

  val treeGenerator: (Int, Int) => Iterator[BooleanExpression] = Generator
    .generate(useConstants = true, Array("a", "b", "c", "d", "e", "f"))

  def parsingAssertion(s: Serialize, d: Deserialize)(exp: BooleanExpression): Assertion = {
    assert(exp equals d(s(exp)))
  }

  test("Test parser (small)") {
    smallCases.foreach(lexerAssert)
  }

  test("Test json (small)") {
    smallCases.foreach(jsonAssert)
  }

  test("Test parser (random 10-depth)") {
    treeGenerator(10, 20).foreach(x => lexerAssert(x))
  }

  test("Test json (random 10-depth)") {
    treeGenerator(10, 20).foreach(x => jsonAssert(x))
  }


}