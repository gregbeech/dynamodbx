package dynamodbx

import java.nio.ByteBuffer

import dynamodbx.impl.PathParser

import scala.util.{Failure, Success}

sealed trait Operand

sealed trait PathSegment
case class Name(get: String) extends PathSegment
case class ListIndex(get: Int) extends PathSegment
case class Path(name: Name, segments: List[PathSegment] = List.empty)

case object Path {
  def apply(path: String): Path = new PathParser(path).path.run() match {
    case Success(p) => p
    case Failure(e) => throw e
  }
}

case class PK(path: Path) extends Operand {
  def eql(operand: Operand): PKCondition = PKCondition(path, Eq(operand))
}

object PK {
  def apply(path: String): PK = PK(Path(path))
}

case class SK(path: Path) extends Operand {
  def eql(operand: Operand): SKCondition = SKCondition(path, Eq(operand))
  def ne(operand: Operand): SKCondition = SKCondition(path, Ne(operand))
  def gt(operand: Operand): SKCondition = SKCondition(path, Gt(operand))
  def lt(operand: Operand): SKCondition = SKCondition(path, Lt(operand))
  def gte(operand: Operand): SKCondition = SKCondition(path, Gte(operand))
  def lte(operand: Operand): SKCondition = SKCondition(path, Lte(operand))
  def beginsWith(operand: Operand): SKCondition = SKCondition(path, BeginsWith(operand))
  def between(lower: Operand, upper: Operand): SKCondition = SKCondition(path, Between(lower, upper))
}

object SK {
  def apply(path: String): SK = SK(Path(path))
}

case class Attr(path: Path) extends Operand {
  def eql(operand: Operand): AttrFilter = AttrFilter(path, Eq(operand))
  def ne(operand: Operand): AttrFilter = AttrFilter(path, Ne(operand))
  def gt(operand: Operand): AttrFilter = AttrFilter(path, Gt(operand))
  def lt(operand: Operand): AttrFilter = AttrFilter(path, Lt(operand))
  def gte(operand: Operand): AttrFilter = AttrFilter(path, Gte(operand))
  def lte(operand: Operand): AttrFilter = AttrFilter(path, Lte(operand))
  def beginsWith(operand: Operand): AttrFilter = AttrFilter(path, BeginsWith(operand))
  def between(lower: Operand, upper: Operand): AttrFilter = AttrFilter(path, Between(lower, upper))
  def contains(operand: Operand): AttrFilter = AttrFilter(path, Contains(operand))
  def exists: AttrFilter = AttrFilter(path, Exists)
  def notExists: AttrFilter = AttrFilter(path, NotExists)
}

object Attr {
  def apply(path: String): Attr = Attr(Path(path))
}

sealed trait Value extends Operand
sealed trait OrderedValue extends Value
sealed trait SequenceValue extends OrderedValue

object Operand {
  implicit def operandValueFromInput[A](a: A)(implicit ev: Input[A]): Value = ev.value(a)
}

case class B(get: ByteBuffer) extends SequenceValue

object B {
  def apply(bytes: Array[Byte]): B = B(ByteBuffer.wrap(bytes))
}

case class BOOL(get: Boolean) extends Value

case class N(get: String) extends OrderedValue

object N {
  def apply[A : Numeric](a: A): N = N(a.toString)
}

case class S(get: String) extends SequenceValue

case class SS(get: Set[String]) extends Value
