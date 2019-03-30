package dynamodbx.json.spray

import dynamodbx.common.{Expression, ExpressionContext}
import dynamodbx._
import spray.json.{JsBoolean, JsNumber, JsObject, JsString, JsValue}

object QueryJson {
  def apply(query: Query): JsObject = {
    implicit val context: ExpressionContext = new ExpressionContext()

    var fields = Map[String, JsValue]()
    query.source match {
      case Table(name) => fields += "TableName" -> JsString(name)
      case Index(index, Table(table)) =>
        fields += "TableName" -> JsString(table)
        fields += "IndexName" -> JsString(index)
    }

    fields += "KeyConditionExpression" -> JsString(Expression(query.condition))
    query.select.foreach {
      case AllAttributes => fields += "Select" -> JsString("ALL_ATTRIBUTES")
      case AllProjectedAttributes => fields += "Select" -> JsString("ALL_PROJECTED_ATTRIBUTES")
      case Count => fields += "Select" -> JsString("COUNT")
      case SpecificAttributes(paths) =>
        fields += "Select" -> JsString("SPECIFIC_ATTRIBUTES")
        fields += "ProjectionExpression" -> JsString(Expression(paths))
    }

    query.order.foreach(order => fields += "ScanIndexForward" -> JsBoolean(order.isForward))
    query.limit.foreach(fields += "Limit" -> JsNumber(_))
    query.exclusiveStartKey.foreach(fields += "ExclusiveStartKey" -> AttributeJson(_))
    query.filter.foreach(filter => fields += "FilterExpression" -> JsString(Expression(filter)))
    query.consistentRead.foreach(fields += "ConsistentRead" -> JsBoolean(_))

    fields += "ExpressionAttributeNames" -> JsObject(context.nameMap.mapValues(JsString(_)))
    fields += "ExpressionAttributeValues" -> JsObject(context.valueMap.mapValues(AttributeJson(_)))
    JsObject(fields)
  }
}
