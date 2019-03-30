package dynamodbx.awssdk.v2

import dynamodbx.common.{Expression, ExpressionContext}
import dynamodbx._
import software.amazon.awssdk.services.dynamodb.model.QueryRequest
import software.amazon.awssdk.services.dynamodb.model.Select._

import scala.collection.JavaConverters._

object QueryRequestBuilder {
  def build(query: Query): QueryRequest = {
    implicit val context: ExpressionContext = new ExpressionContext()

    val request = mkRequest(query.source)
      .keyConditionExpression(Expression(query.condition))

    query.select.foreach {
      case AllAttributes => request.select(ALL_ATTRIBUTES)
      case AllProjectedAttributes => request.select(ALL_PROJECTED_ATTRIBUTES)
      case Count => request.select(COUNT)
      case SpecificAttributes(paths) =>
        request.select(SPECIFIC_ATTRIBUTES)
        request.projectionExpression(Expression(paths))
    }

    query.filter.foreach { filter =>
      request.filterExpression(Expression(filter))
    }

    query.order.foreach(order => request.scanIndexForward(order.isForward))
    query.limit.foreach(request.limit(_))

    request.expressionAttributeNames(context.nameMap.asJava)
    request.expressionAttributeValues(context.valueMap.mapValues(AttributeValueBuilder.build).asJava)
    request.build()
  }

  private def mkRequest(source: Source): QueryRequest.Builder = source match {
    case Table(name) => QueryRequest.builder.tableName(name)
    case Index(name, table) => mkRequest(table).indexName(name)
  }
}
