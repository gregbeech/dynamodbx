package dynamodbx

import java.time.Instant

import cats.Functor
import shapeless.{::, Generic, HList, HNil}

trait Output[A] {
  def get(v: Value): A
}

object Output extends LowPriorityOutputImplicits {
  implicit val functorInstanceForOutput: Functor[Output] = new Functor[Output] {
    override def map[A, C](fa: Output[A])(f: A => C): Output[C] = v => f(fa.get(v))
  }

  implicit val booleanOutput: Output[Boolean] = fromPF { case BOOL(b) => b }
  implicit val doubleOutput: Output[Double] = fromPF { case N(n) => n.toDouble }
  implicit val intOutput: Output[Int] = fromPF { case N(n) => n.toInt }
  implicit val stringOutput: Output[String] = fromPF { case S(s) => s }
  implicit val stringSetOutput: Output[Set[String]] = fromPF { case SS(ss) => ss }

  def apply[A](implicit o: Output[A]): Output[A] = o

  def fromPF[A](pf: PartialFunction[Value, A]): Output[A] = value =>
    if (pf.isDefinedAt(value)) pf(value)
    else throw new IllegalArgumentException(s"Attribute is of type ${value.getClass.getSimpleName}")
}

trait LowPriorityOutputImplicits {
  implicit val instantOutput: Output[Instant] = Output.fromPF {
    case N(n) => Instant.ofEpochSecond(n.toLong)
  }
}

final class Read[A](val outputs: List[Output[_]], val unsafeRead: (Map[String, Value], List[String]) => A)

object Read extends LowPriorityReadImplicits {
  implicit def fromOutput[A](implicit output: Output[A]): Read[A] =
    new Read[A](List(output), (vs, ns) => output.get(vs(ns.head)))

  def apply[A](implicit r: Read[A]): Read[A] = r
}

trait LowPriorityReadImplicits {
  implicit def product[H, T <: HList](implicit H: Read[H], T: Read[T]): Read[H :: T] = new Read(
    H.outputs ++ T.outputs,
    (vs, ns) => H.unsafeRead(vs, List(ns.head)) :: T.unsafeRead(vs, ns.tail)
  )

  implicit def emptyProduct: Read[HNil] = new Read[HNil](Nil, (_, _) => HNil)

  implicit def generic[A, R](implicit gen: Generic.Aux[A, R], G: Read[R]): Read[A] =
    new Read[A](G.outputs, (vs, n) => gen.from(G.unsafeRead(vs, n)))
}
