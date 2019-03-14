package dynamodbx.awssdk.v2

import cats.implicits._
import dynamodbx._
import dynamodbx.awssdk.v2.syntax._
import org.scalatest.{MustMatchers, WordSpec}

class QueryRequestBuilderSpec extends WordSpec with MustMatchers {

  case class WidgetId(get: String)

  implicit val widgetIdInput: Input[WidgetId] = Input[String].contramap(id => s"Widget#${id.get}")

  "stuff" in {
    val table = Table("Widgets").index("GSI1")
    val q = Query(table, PK("MyId").eql(WidgetId("123"))).filter(Attr("MyAttr.MyMapVal[0]").beginsWith("foo"))
    val r = q.toRequest
    r mustEqual null
  }

}
