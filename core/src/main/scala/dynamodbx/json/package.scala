package dynamodbx

import dynamodbx.common.Builder
import spray.json.JsObject

package object json {
  type JsonBuilder[A] = Builder[A, JsObject]
}
