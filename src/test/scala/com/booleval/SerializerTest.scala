package com.booleval

import org.scalatest.FunSuite
import play.api.libs.json.Json

class SerializerTest extends FunSuite {

  test("Five level tree") {
    val be =  BooleanExpressionGenerator.generate(1, Array("a", "b"), useConstants = true)
    val json = Serial.serialize(be)
    val be2 = Serial.deserialize(json)
    assert(be2 equals be)
  }



}