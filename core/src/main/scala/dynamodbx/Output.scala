package dynamodbx

import cats.Functor
import shapeless.{::, Generic, HList, HNil}

trait Output[A] {
  def get(v: Value): A
}

object Output {
  implicit val functorInstanceForOutput: Functor[Output] = new Functor[Output] {
    override def map[A, C](fa: Output[A])(f: A => C): Output[C] = v => f(fa.get(v))
  }

  implicit val booleanOutput: Output[Boolean] = output { case BOOL(b) => b }
  implicit val doubleOutput: Output[Double] = output { case N(n) => n.toDouble }
  implicit val intOutput: Output[Int] = output { case N(n) => n.toInt }
  implicit val stringOutput: Output[String] = output { case S(s) => s }

  def apply[A](implicit o: Output[A]): Output[A] = o

  private def output[A](pf: PartialFunction[Value, A])(value: Value): A =
    if (pf.isDefinedAt(value)) pf(value)
    else throw new IllegalArgumentException(s"Attribute is of type ${value.getClass.getSimpleName}")
}

final class Read[A](val outputs: List[Output[_]], val unsafeRead: (List[Value], Int) => A)

object Read extends LowPriorityReadImplicits {
  implicit def fromOutput[A](implicit output: Output[A]): Read[A] =
    new Read[A](List(output), (vs, n) => output.get(vs(n)))

  def apply[A](implicit r: Read[A]): Read[A] = r
}

trait LowPriorityReadImplicits {
  implicit def product[H, T <: HList](implicit H: Read[H], T: Read[T]): Read[H :: T] = new Read(
    H.outputs ++ T.outputs,
    (vs, n) => H.unsafeRead(vs, n) :: T.unsafeRead(vs, n + H.outputs.length)
  )

  implicit def emptyProduct: Read[HNil] = new Read[HNil](Nil, (_, _) => HNil)

  implicit def generic[A, R](implicit gen: Generic.Aux[A, R], G: Read[R]): Read[A] =
    new Read[A](G.outputs, (vs, n) => gen.from(G.unsafeRead(vs, n)))
}

