package zio.web.websockets.codec

import zio.Chunk
import Bits._

sealed trait Codec[A] { self =>
  def ~[B](that: Codec[B])(implicit z: Zippable[A, B]): Codec[z.Out] = self.zip(that)

  def <~(that: Codec[Unit]): Codec[A] = self.zip(that).transform(_._1, a => (a, ()))

  def orElseEither[B](that: Codec[B]): Codec[Either[A, B]] =
    Codec.OrElseEither(self, that)

  def toUnit(defaultValue: A): Codec[Unit] = self.transform(_ => (), _ => defaultValue)

  def transform[B](f: A => B, g: B => A): Codec[B] =
    Codec.Transform(self, f, g)

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Codec[B] =
    Codec.TransformOrFail(self, f, g)

  def zip[B](that: Codec[B])(implicit z: Zippable[A, B]): Codec[z.Out] =
    Codec.Zip(z, self, that)

  def sizeBytes(f: A => Int): Codec[(A, Bits)] = Codec.SizeBits(self, f)

  def optional: Codec[Option[A]] = Codec.Optional(self)

  def encode(a: A): Either[String, Bits] = ???

  def decode(bits: Bits): Either[String, A] = ???
}

object Codec {
  implicit class CodecUnitSyntax(self: Codec[Unit]) {
    def as[B](b: B): Codec[B] = self.transform(_ => b, _ => ())

    def ~>[B](that: Codec[B]): Codec[B] =
      self.zip(that).transform(_._2, b => ((), b))
  }

  final private case class ReadBitRange(min: Int, max: Int)                           extends Codec[Bits]
  final private case class ReadBits(bits: Int)                                        extends Codec[Bits]
  final private case class Transform[A, B](codec: Codec[A], to: A => B, from: B => A) extends Codec[B]
  final private case class TransformOrFail[A, B](
    codec: Codec[A],
    to: A => Either[String, B],
    from: B => Either[String, A]
  ) extends Codec[B]
  final private case class Zip[A, B, Z](z: Zippable[A, B], left: Codec[A], right: Codec[B]) extends Codec[Z]
  final private case class OrElseEither[A, B](left: Codec[A], right: Codec[B])              extends Codec[Either[A, B]]
  final private case class Optional[A](codec: Codec[A])                                     extends Codec[Option[A]]
  final private case class SizeBits[A](codec: Codec[A], sizeOf: A => Int)                   extends Codec[(A, Bits)]

  def apply[A](value: A): Codec[A] = ReadBits(0).transform(_ => value, _ => Chunk.empty)

  val bit: Codec[Bits] = ReadBits(1)

  def bits(n: Int): Codec[Bits] = ReadBits(n)

  def bitRange(min: Int, max: Int): Codec[Bits] = ReadBitRange(min, max)

  val byte: Codec[Bits] = ReadBits(8)

  def fail[A](message: String): Codec[A] = fail(message, message)

  def fail[A](left: String, right: String): Codec[A] = unit.transformOrFail(_ => Left(left), _ => Left(right))

  def literalAll(expected: Bits): Codec[Unit] =
    Codec
      .bits(expected.length)
      .transformOrFail(
        bits => if (expected == bits) Right(()) else Left(s"Expected ${expected} but found ${bits}"),
        bits => if (expected == bits) Right(expected) else Left(s"Expected ${expected} but found ${bits}")
      )

  def literal(b1: Boolean): Codec[Unit] = literalAll(Chunk(b1))

  def literal(b1: Boolean, b2: Boolean): Codec[Unit] = literalAll(Chunk(b1, b2))

  def literal(b1: Boolean, b2: Boolean, b3: Boolean, b4: Boolean): Codec[Unit] =
    literalAll(Chunk(b1, b2, b3, b4))

  // ETC

  val one: Codec[Unit] = bits(1).toUnit(Chunk(true))

  val unit: Codec[Unit] = bits(0).toUnit(Chunk.empty)

  val word: Codec[Bits] = ReadBits(16)

  val short: Codec[Short] = bitRange(0, 15).transform(_.toShort.get, Bits.fromShort(_))

  val int: Codec[Int] = bitRange(0, 31).transform(_.toInt.get, Bits.fromInt(_))

  val boolean: Codec[Boolean] = bits(1).transform(bits => bits(0), b => Bits.fromByte(if (b) 0x80.toByte else 0))
}
