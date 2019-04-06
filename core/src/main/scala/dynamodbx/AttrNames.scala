package dynamodbx

import scala.reflect.runtime.universe._

sealed trait Capitalisation {
  def capitalise(s: String): String
}
case object PascalCase extends Capitalisation {
  override def capitalise(s: String): String = s.capitalize
}
case object CamelCase extends Capitalisation {
  override def capitalise(s: String): String = s
}

object Capitalisation extends LowPriorityCapitalisationImplicits

trait LowPriorityCapitalisationImplicits {
  implicit val pascalCase: Capitalisation = PascalCase
}

sealed trait AttrNames[A] {
  def names: List[String]
}

object AttrNames {
  implicit def attrNamesForCaseClass[A : TypeTag](implicit capitalisation: Capitalisation): AttrNames[A] = new AttrNames[A] {
    override def names: List[String] = typeOf[A].members.sorted.collect {
      case m: MethodSymbol if m.isCaseAccessor => capitalisation.capitalise(m.name.toString)
    }
  }

  def apply[A](implicit an: AttrNames[A]): AttrNames[A] = an
}
