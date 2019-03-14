package dynamodbx.awssdk.v2

import dynamodbx._
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.collection.JavaConverters._

private[v2] object AttributeValueBuilder {
  def build(value: Value): AttributeValue = value match {
    case B(b) => AttributeValue.builder.b(SdkBytes.fromByteBuffer(b)).build
    case BOOL(bool) => AttributeValue.builder.bool(bool).build
    case N(n) => AttributeValue.builder.n(n).build
    case S(s) => AttributeValue.builder.s(s).build
    case SS(ss) => AttributeValue.builder.ss(ss.asJavaCollection).build
  }
}
