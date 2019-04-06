package dynamodbx.awssdk.v2

import dynamodbx._
import dynamodbx.common.Builder
import software.amazon.awssdk.core.SdkBytes
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.collection.JavaConverters._

object AttributeValueBuilder extends Builder[Value, AttributeValue] {
  def apply(value: Value): AttributeValue = build(value, 0)

  def from(attr: AttributeValue): Value =
    if (attr.s != null) S(attr.s)
    else if (attr.n != null) N(attr.n)
    else if (attr.bool != null) BOOL(attr.bool)
    else if (attr.ss.size > 0) SS(attr.ss.asScala.toSet)
    else if (attr.ns.size > 0) NS(attr.ns.asScala.toSet)
    // TODO: remaining cases
    else if (attr.nul) throw new IllegalArgumentException("Attribute value is NULL")
    else throw new UnsupportedOperationException("Type of attribute value is not supported.")

  private def build(value: Value, depth: Int): AttributeValue = {
    if (depth > 32) throw new IllegalArgumentException("Values cannot be more than 32 levels deep")
    value match {
      case B(b) => AttributeValue.builder.b(SdkBytes.fromByteBuffer(b)).build
      case BOOL(bool) => AttributeValue.builder.bool(bool).build
      case L(xs) => AttributeValue.builder.l(xs.map(x => build(x, depth + 1)).asJavaCollection).build
      case M(m) => AttributeValue.builder.m(m.mapValues(v => build(v, depth + 1)).asJava).build
      case N(n) => AttributeValue.builder.n(n).build
      case NS(ns) => AttributeValue.builder.ns(ns.asJavaCollection).build
      case S(s) => AttributeValue.builder.s(s).build
      case SS(ss) => AttributeValue.builder.ss(ss.asJavaCollection).build
    }
  }
}
