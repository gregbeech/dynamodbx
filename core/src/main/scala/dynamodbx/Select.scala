package dynamodbx

sealed trait Select
case object AllAttributes extends Select
case object AllProjectedAttributes extends Select
case class SpecificAttributes(attributes: Seq[Path]) extends Select
case object Count extends Select
