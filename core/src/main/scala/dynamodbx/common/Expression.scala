package dynamodbx.common

import dynamodbx._

object Expression {
  def apply(condition: Condition)(implicit context: ExpressionContext): String = condition match {
    case c: PKCondition => format(c: AttrComparison)
    case KeyCondition(pk, sk) => s"(${format(pk: AttrComparison)}) and (${format(sk)})"
  }

  def apply(filter: Filter)(implicit context: ExpressionContext): String = filter match {
    case c: AttrFilter => format(c: AttrComparison)
    case And(left, right) => s"(${apply(left)}) and (${apply(right)})"
    case Or(left, right) => s"(${apply(left)}) or (${apply(right)})"
    case Not(cond) => s"not (${apply(cond)})"
  }

  def apply(paths: Seq[Path])(implicit context: ExpressionContext): String =
    paths.map(format).mkString(", ")

  private def format(comparison: AttrComparison)(implicit context: ExpressionContext): String = {
    val path = format(comparison.path)
    comparison.operation match {
      case Eq(operand) => s"$path = ${format(operand)}"
      case Ne(operand) => s"$path <> ${format(operand)}"
      case Gt(operand) => s"$path > ${format(operand)}"
      case Lt(operand) => s"$path < ${format(operand)}"
      case Gte(operand) => s"$path >= ${format(operand)}"
      case Lte(operand) => s"$path <= ${format(operand)}"
      case BeginsWith(operand) => s"$path begins_with(${format(operand)})"
      case Between(lower, upper) => s"$path between ${format(lower)} and ${format(upper)}"
      case Contains(operand) => s"contains($path, ${format(operand)})"
      case Exists => s"attribute_exists($path)"
      case NotExists => s"attribute_not_exists($path)"
    }
  }

  private def format(operand: Operand)(implicit context: ExpressionContext): String = operand match {
    case value: Value => format(value)
    case PK(path) => format(path)
    case SK(path) => format(path)
    case Attr(path) => format(path)
  }

  private def format(path: Path)(implicit context: ExpressionContext): String =
    (context.name(path.name.get) :: path.segments.map(format)).mkString(".")

  private def format(segment: PathSegment)(implicit context: ExpressionContext): String = segment match {
    case Name(name) => context.name(name)
    case ListIndex(index) => s"[$index]"
  }

  private def format(value: Value)(implicit context: ExpressionContext): String = context.value(value)
}
