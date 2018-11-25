package com.booleval

import play.api.libs.json.Reads._
import play.api.libs.json._

object Serial {

  private type BiOperatorFactory[T] = (BooleanExpression, BooleanExpression) => T

  private implicit lazy val serializer: Writes[BooleanExpression] = {
    case True => Json.obj("type" -> "true")
    case False => Json.obj("type" -> "false")
    case Variable(symbol) => Json.obj("type" -> "var", "symbol" -> symbol)
    case Not(exp) => Json.obj("type" -> "not", "exp" -> Json.toJson(exp))
    case Or(lhs, rhs) => Json.obj("type" -> "or", "lhs" -> Json.toJson(lhs), "rhs" -> Json.toJson(rhs))
    case And(lhs, rhs) => Json.obj("type" -> "and", "lhs" -> Json.toJson(lhs), "rhs" -> Json.toJson(rhs))
  }

  private def biOpReadsFactory[T](factory: BiOperatorFactory[T]): Reads[T] = {
    json: JsValue =>
      (JsPath \ "lhs").read[JsObject].reads(json) match {
        case JsSuccess(lhs, _) => (JsPath \ "rhs").read[JsObject].reads(json) match {
          case JsSuccess(rhs, _) => JsSuccess(factory.apply(lhs.validate[BooleanExpression].get, rhs.validate[BooleanExpression].get))
          case JsError(err) => JsError(err)
        }
        case JsError(err) => JsError(err)
      }
  }


  private implicit lazy val readsAnd: Reads[And] = biOpReadsFactory(And)

  private implicit lazy val readsOr: Reads[Or] = biOpReadsFactory(Or)

  private implicit lazy val readNot: Reads[Not] = json => (JsPath \ "exp").read[JsObject].reads(json) match {
    case JsSuccess(value, _) => JsSuccess(Not(value.validate[BooleanExpression].get))
    case JsError(err) => JsError(err)
  }

  private implicit val readsVariable: Reads[Variable] = json => (JsPath \ "symbol").read[String].reads(json) match {
    case JsSuccess(value, _) => JsSuccess(Variable(value))
    case JsError(err) => JsError(err)
  }

  private implicit lazy val deserializer: Reads[BooleanExpression] = json => (JsPath \ "type").read[String].reads(json) match {
    case JsSuccess(value, _) => value match {
      case "true" => JsSuccess(True)
      case "false" => JsSuccess(False)
      case "var" => json.validate[Variable]
      case "and" => json.validate[And]
      case "or" => json.validate[Or]
      case "not" => json.validate[Not]
      case _ => JsError()
    }
    case JsError(errors) => JsError(errors)
  }

  def serialize(be: BooleanExpression): JsValue = {
    Json.toJson(be)
  }

  def deserialize(js: JsValue): BooleanExpression = {
    js.validate[BooleanExpression].get
  }

}
