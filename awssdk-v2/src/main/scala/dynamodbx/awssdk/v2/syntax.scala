package dynamodbx.awssdk.v2

import dynamodbx.{AttrNames, Query, Read}
import software.amazon.awssdk.services.dynamodb.model.{QueryRequest, QueryResponse}

object syntax {
  implicit class RichQuery(private val query: Query) extends AnyVal {
    def asRequest: QueryRequest = QueryRequestBuilder(query)
  }

  implicit class RichQueryResponse(private val response: QueryResponse) extends AnyVal {
    def asList[A : Read : AttrNames]: List[A] = QueryResponseParser[A](response)
  }

}
