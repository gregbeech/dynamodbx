package dynamodbx

sealed trait Select
sealed trait TableSelect extends Select
sealed trait IndexSelect extends Select
case object AllAttributes extends TableSelect with IndexSelect
case object AllProjectedAttributes extends IndexSelect
case class SpecificAttributes(attributes: Seq[Path]) extends TableSelect with IndexSelect
case object Count extends TableSelect with IndexSelect
