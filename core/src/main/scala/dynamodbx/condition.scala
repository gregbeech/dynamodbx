package dynamodbx

sealed trait Operation
sealed trait SKOperation extends Operation
sealed trait PKOperation extends Operation
case class Eq(operand: Operand) extends PKOperation with SKOperation
case class Ne(operand: Operand) extends SKOperation
case class Gt(operand: Operand) extends SKOperation
case class Lt(operand: Operand) extends SKOperation
case class Gte(operand: Operand) extends SKOperation
case class Lte(operand: Operand) extends SKOperation
case class BeginsWith(operand: Operand) extends SKOperation
case class Between(lower: Operand, upper: Operand) extends SKOperation
case class Contains(operand: Operand) extends Operation
case object Exists extends Operation
case object NotExists extends Operation

sealed trait AttrComparison {
  val path: Path
  val operation: Operation
}

sealed trait Condition
case class PKCondition(path: Path, operation: PKOperation) extends Condition with AttrComparison {
  def and(sk: SKCondition): KeyCondition = KeyCondition(this, sk)
}
case class SKCondition(path: Path, operation: SKOperation) extends AttrComparison
case class KeyCondition(pk: PKCondition, sk: SKCondition) extends Condition

sealed trait Filter
case class And(left: Filter, right: Filter) extends Filter
case class Or(left: Filter, right: Filter) extends Filter
case class Not(condition: Filter) extends Filter
case class AttrFilter(path: Path, operation: Operation) extends Filter with AttrComparison
