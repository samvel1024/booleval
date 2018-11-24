package com.booleval

import org.scalatest.FunSuite
import play.api.libs.json.Json

class SerializerTest extends FunSuite {

  test("Five level tree") {
    val be =  BooleanExpressionGenerator.generate(15, Array[String]("a", "b"))
    val json = Serial.serialize(be)
    val be2 = Serial.deserialize(json)

    println(Json.prettyPrint(json))
    println("_______________________________________________")
    println(Json.prettyPrint(Serial.serialize(be2)))
    assert(be2 equals be)
  }



}