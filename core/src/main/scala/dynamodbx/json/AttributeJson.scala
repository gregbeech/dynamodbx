package dynamodbx.json

import dynamodbx.{B, BOOL, L, M, N, NS, S, SS, Value}
import spray.json.{JsArray, JsBoolean, JsNumber, JsObject, JsString}

object AttributeJson extends JsonBuilder[Value] {
  def apply(value: Value): JsObject = format(value, 0)

  private def format(value: Value, depth: Int): JsObject = {
    if (depth > 32) throw new IllegalArgumentException("Values cannot be more than 32 levels deep")
    value match {
      case B(b) => JsObject("B" -> JsArray(b.array.map(x => JsNumber(x.toInt)).toVector))
      case BOOL(bool) => JsObject("BOOL" -> JsBoolean(bool))
      case L(xs) => JsObject("L" -> JsArray(xs.map(x => format(x, depth + 1)).toVector))
      case M(m) => JsObject("M" -> JsObject(m.mapValues(v => format(v, depth + 1))))
      case N(n) => JsObject("N" -> JsNumber(n))
      case NS(ns) => JsObject("NS" -> JsArray(ns.map(JsString(_)).toVector))
      case S(s) => JsObject("S" -> JsString(s))
      case SS(ss) => JsObject("SS" -> JsArray(ss.map(JsString(_)).toVector))
    }
  }
}
