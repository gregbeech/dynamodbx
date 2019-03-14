package dynamodbx

sealed trait Source

case class Table(name: String) extends Source {
  def index(name: String): Index = Index(name, this)
}

case class Index(name: String, table: Table) extends Source

trait Query {
  val source: Source
  val condition: Condition
  val select: Select
  val filter: Filter
}

case class TableQuery(table: Table, condition: Condition, select: TableSelect, filter: Filter) extends Query {
  override val source: Source = table
  def filter(f: Filter): TableQuery = copy(filter = f)
  def select(s: TableSelect): TableQuery = copy(select = s)
}
case class IndexQuery(index: Index, condition: Condition, select: IndexSelect, filter: Filter) extends Query {
  override val source: Source = index
  def filter(f: Filter): IndexQuery = copy(filter = f)
  def select(s: IndexSelect): IndexQuery = copy(select = s)
}

object Query {
  def apply(table: Table, condition: Condition): TableQuery = TableQuery(table, condition, AllAttributes, NoFilter)
  def apply(index: Index, condition: Condition): IndexQuery = IndexQuery(index, condition, AllProjectedAttributes, NoFilter)
}
