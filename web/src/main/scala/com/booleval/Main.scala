package com.booleval

object Main extends App {
  val be = Or(And(True, False), True)
  println(Serial.serialize(be))
}
