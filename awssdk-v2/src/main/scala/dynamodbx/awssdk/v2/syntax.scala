package dynamodbx.awssdk.v2

import dynamodbx.Query
import software.amazon.awssdk.services.dynamodb.model.QueryRequest

object syntax {
  implicit class RichQuery(private val query: Query) extends AnyVal {
    def toRequest: QueryRequest = QueryRequestBuilder.build(query)
  }
}
