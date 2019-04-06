package dynamodbx.awssdk.v2

import cats.implicits._
import dynamodbx._
import dynamodbx.awssdk.v2.syntax._
import org.scalatest.{MustMatchers, WordSpec}
import software.amazon.awssdk.services.dynamodb.model.AttributeValue

import scala.collection.JavaConverters._

class QueryRequestBuilderSpec extends WordSpec with MustMatchers {

  case class WidgetId(get: String)
  case class RowVersion(get: Int)
  case class Category(name: String)

  implicit val widgetIdInput: Input[WidgetId] = Input[String].contramap(id => s"Widget#${id.get}")
  implicit val rowVersionInput: Input[RowVersion] = Input[String].contramap(v => f"v_${v.get}%06d")
  implicit val categoryInput: Input[Category] = Input[String].contramap(_.name)

  "stuff" in {
    val query = Query(
      Table("Widgets").index("GSI1"),
      (PK("Id") eql WidgetId("123")) and (SK("Version") eql RowVersion(0)),
//      select = Some(SpecificAttributes(Seq(Path("Foo"), Path("Bar"), Path("Baz")))), // TODO: nicer syntax
      filter = Some(Attr("Category") eql Category("things"))
    )

    println(query.toString)

    val request = query.asRequest
    request.tableName mustEqual "Widgets"
    request.indexName mustEqual "GSI1"
    request.keyConditionExpression mustEqual "(#0 = :0) and (#1 = :1)"
    request.filterExpression mustEqual "#2 = :2"
    request.expressionAttributeNames.asScala mustEqual Map(
      "#0" -> "Id",
      "#1" -> "Version",
      "#2" -> "Category"
    )
    request.expressionAttributeValues.asScala mustEqual Map(
      ":0" -> AttributeValue.builder.s("Widget#123").build,
      ":1" -> AttributeValue.builder.s("v_000000").build,
      ":2" -> AttributeValue.builder.s("things").build
    )
  }

}
