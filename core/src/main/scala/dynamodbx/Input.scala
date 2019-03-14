package dynamodbx

import java.nio.ByteBuffer

import cats._
import cats.implicits._

trait Input[A] {
  def value(a: A): Value
}

object Input {
  implicit val contravariantInstanceForInput: Contravariant[Input] = new Contravariant[Input] {
    override def contramap[A, C](fa: Input[A])(f: C => A): Input[C] = c => fa.value(f(c))
  }

  implicit val booleanInput: Input[Boolean] = a => BOOL(a)
  implicit val byteBufferInput: Input[ByteBuffer] = a => B(a)
  implicit val byteArrayInput: Input[Array[Byte]] = byteBufferInput.contramap(ByteBuffer.wrap)
  implicit val bigDecimalInput: Input[BigDecimal] = a => N(a.toString)
  implicit val bigIntInput: Input[BigInt] = a => N(a.toString)
  implicit val byteInput: Input[Byte] = a => N(a.toString)
  implicit val doubleInput: Input[Double] = a => N(a.toString)
  implicit val floatInput: Input[Float] = a => N(a.toString)
  implicit val intInput: Input[Int] = a => N(a.toString)
  implicit val longInput: Input[Long] = a => N(a.toString)
  implicit val stringInput: Input[String] = a => S(a)
  implicit val stringSetInput: Input[Set[String]] = a => SS(a)

  def apply[A](implicit a: Input[A]): Input[A] = a
}
