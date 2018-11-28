package com.booleval

import org.scalatest.{Assertion, FunSuite}
import play.api.libs.json.Json
import BooleanExpressionImplicit._

class SerializationTest extends FunSuite {

  type Serialize = BooleanExpression => String
  type Deserialize = String => BooleanExpression

  def parsingAssertion(s: Serialize, d: Deserialize)(exp: BooleanExpression): Assertion = {
    assert(exp equals d(s(exp)))
  }

  val jsonAssert: BooleanExpression => Unit = parsingAssertion(
    exp => JsonSerial.serialize(exp).toString(),
    str => JsonSerial.deserialize(Json.parse(str)))

  val lexerAssert: BooleanExpression => Unit = parsingAssertion(
    e => e.toString,
    s => Parser.parse(s))

  val smallCases = List(!(~"a"), !True, !False, False, True, ~"b", True | False, ~"a" & !(~"b"), False & !True)

  test("Test parser (small)"){
    smallCases.foreach(lexerAssert)
  }

  test("Test json (small)"){
    smallCases.foreach(jsonAssert)
  }

  test("Test parser (random 10-depth)"){
    val be =  Generator.generate(10, Array("a", "b", "c", "d", "e", "f"), useConstants = true)
    lexerAssert(be)
  }

  test("Test json (random 10-depth)"){
    val be =  Generator.generate(1, Array("a", "b"), useConstants = true)
    jsonAssert(be)
  }




}