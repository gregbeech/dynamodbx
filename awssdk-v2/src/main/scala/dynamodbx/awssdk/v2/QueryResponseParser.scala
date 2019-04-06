package dynamodbx.awssdk.v2

import dynamodbx.{AttrNames, Read}
import software.amazon.awssdk.services.dynamodb.model.QueryResponse

import scala.collection.JavaConverters._

object QueryResponseParser {
  def apply[A](response: QueryResponse)(implicit read: Read[A], attr: AttrNames[A]): List[A] =
    response.items.asScala.map { item =>
      val values = item.asScala.mapValues(AttributeValueBuilder.from).toMap
      read.unsafeRead(values, attr.names)
    }.toList
}
