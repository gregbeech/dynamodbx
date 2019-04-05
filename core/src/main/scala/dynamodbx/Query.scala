package dynamodbx

import dynamodbx.json.QueryJson

sealed trait Order {
  val isForward: Boolean
}
case object Asc extends Order {
  override val isForward: Boolean = true
}
case object Desc extends Order {
  override val isForward: Boolean = false
}

sealed trait Source

case class Table(name: String) extends Source {
  def index(name: String): Index = Index(name, this)
}

case class Index(name: String, table: Table) extends Source

case class Query(
  source: Source,
  condition: Condition,
  select: Option[Select] = None,
  order: Option[Order] = None,
  limit: Option[Int] = None,
  exclusiveStartKey: Option[OrderedValue] = None, // not quite right; should be S or N or B only
  filter: Option[Filter] = None,
  consistentRead: Option[Boolean] = None
) {
  override def toString: String = QueryJson(this).sortedPrint
}
