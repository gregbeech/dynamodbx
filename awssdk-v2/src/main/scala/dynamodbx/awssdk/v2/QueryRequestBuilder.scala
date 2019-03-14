package dynamodbx.awssdk.v2

import dynamodbx.{Index, Query, Source, Table}
import dynamodbx.impl.{ExpressionVisitor, SubstitutionContext}
import software.amazon.awssdk.services.dynamodb.model.QueryRequest

import scala.collection.JavaConverters._

object QueryRequestBuilder {
  def build(query: Query): QueryRequest = {
    implicit val context: SubstitutionContext = new SubstitutionContext()

    builder(query.source)
      .keyConditionExpression(ExpressionVisitor.visit(query.condition))
      .filterExpression(ExpressionVisitor.visit(query.filter))
      .expressionAttributeNames(context.nameMap.asJava)
      .expressionAttributeValues(context.valueMap.mapValues(AttributeValueBuilder.build).asJava)
      .build()
  }

  private def builder(source: Source): QueryRequest.Builder = source match {
    case Table(name) => QueryRequest.builder.tableName(name)
    case Index(name, table) => builder(table).indexName(name)
  }
}
