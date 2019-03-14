package dynamodbx.impl

import dynamodbx._

object ExpressionVisitor {
  def visit(condition: Condition)(implicit context: SubstitutionContext): String = condition match {
    case c: PKCondition => visit(c: AttrComparison)
    case KeyCondition(pk, sk) => s"(${visit(pk: AttrComparison)}) and (${visit(sk)})"
  }

  def visit(filter: Filter)(implicit context: SubstitutionContext): String = filter match {
    case NoFilter => ""
    case c: AttrFilter => visit(c: AttrComparison)
    case And(left, right) => s"(${visit(left)}) and (${visit(right)})"
    case Or(left, right) => s"(${visit(left)}) or (${visit(right)})"
    case Not(cond) => s"not (${visit(cond)})"
  }

  private def visit(comparison: AttrComparison)(implicit context: SubstitutionContext): String = {
    val path = visit(comparison.path)
    comparison.operation match {
      case Eq(operand) => s"$path = ${visit(operand)}"
      case Ne(operand) => s"$path <> ${visit(operand)}"
      case Gt(operand) => s"$path > ${visit(operand)}"
      case Lt(operand) => s"$path < ${visit(operand)}"
      case Gte(operand) => s"$path >= ${visit(operand)}"
      case Lte(operand) => s"$path <= ${visit(operand)}"
      case BeginsWith(operand) => s"$path begins_with(${visit(operand)})"
      case Between(lower, upper) => s"$path between ${visit(lower)} and ${visit(upper)}"
      case Contains(operand) => s"contains($path, ${visit(operand)})"
      case Exists => s"attribute_exists($path)"
      case NotExists => s"attribute_not_exists($path)"
    }
  }

  private def visit(operand: Operand)(implicit context: SubstitutionContext): String = operand match {
    case value: Value => visit(value)
    case PK(path) => visit(path)
    case SK(path) => visit(path)
    case Attr(path) => visit(path)
  }

  private def visit(path: Path)(implicit context: SubstitutionContext): String =
    (context.name(path.name.get) :: path.segments.map(visit)).mkString(".")

  private def visit(segment: PathSegment)(implicit context: SubstitutionContext): String = segment match {
    case Name(name) => context.name(name)
    case ListIndex(index) => s"[$index]"
  }

  private def visit(value: Value)(implicit context: SubstitutionContext): String = context.value(value)
}
