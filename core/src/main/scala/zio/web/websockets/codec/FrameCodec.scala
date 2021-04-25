package zio.web.websockets.codec

import zio.Chunk

sealed private[websockets] trait FrameCodec[A] { self =>
  import FrameCodec._

  def <*>[B](that: FrameCodec[B])(implicit z: Zippable[A, B]): FrameCodec[z.Out] =
    self.zip(that)

  def zip[B](that: FrameCodec[B])(implicit z: Zippable[A, B]): FrameCodec[z.Out] =
    self.flatMap(a => that.map(b => z.zip(a, b)))

  def transform[B](f: A => B, g: B => A): FrameCodec[B] = ???

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): FrameCodec[B] = ???

  def map[B](f: A => B): FrameCodec[B] = ???

  def flatMap[B](f: A => FrameCodec[B]): FrameCodec[B] = ???

  def encode(a: A): Either[String, BitChunk] = ???

  def decode(bits: BitChunk): Either[String, A] = ???
}

private[websockets] object FrameCodec {
  def apply[A](encode: A => BitChunk, decode: BitChunk => A): FrameCodec[A] = ???

  val bit: FrameCodec[BitChunk] =
    FrameCodec[BitChunk](identity, identity)

  val byte: FrameCodec[Byte] =
    FrameCodec[Byte](byte => BitChunk.bytes(byte), chunk => chunk.toByte)

  val bytes: FrameCodec[Chunk[Byte]] =
    FrameCodec[Chunk[Byte]](bs => BitChunk.bytes(bs), chunk => chunk.bytes)

  val int: FrameCodec[Int] =
    FrameCodec[Int](int => BitChunk.int(int), chunk => chunk.toInt)

  val string: FrameCodec[String] =
    bytes.transform(
      bs => new String(bs.toArray, "UTF-8"),
      s => Chunk.fromArray(s.getBytes("UTF-8"))
    )

  def bits(length: Int): FrameCodec[BitChunk] = ???

  def bitsAtLeast(n: Int): FrameCodec[BitChunk] = ???

  sealed trait Zippable[-A, -B] {
    type Out

    def zip(left: A, right: B): Out
  }

  object Zippable extends ImplicitsLowPriority {
    type Out[A, B, Z] = Zippable[A, B] { type Out = Z }

    implicit def Zippable3[A1, A2, Z]: Zippable.Out[(A1, A2), Z, (A1, A2, Z)] =
      new Zippable[(A1, A2), Z] {
        type Out = (A1, A2, Z)

        override def zip(left: (A1, A2), right: Z): (A1, A2, Z) = (left._1, left._2, right)
      }

    implicit def Zippable4[A1, A2, A3, Z]: Zippable.Out[(A1, A2, A3), Z, (A1, A2, A3, Z)] =
      new Zippable[(A1, A2, A3), Z] {
        type Out = (A1, A2, A3, Z)

        override def zip(left: (A1, A2, A3), right: Z): (A1, A2, A3, Z) =
          (left._1, left._2, left._3, right)
      }

    implicit def Zippable5[A1, A2, A3, A4, Z]: Zippable.Out[(A1, A2, A3, A4), Z, (A1, A2, A3, A4, Z)] =
      new Zippable[(A1, A2, A3, A4), Z] {
        type Out = (A1, A2, A3, A4, Z)

        override def zip(left: (A1, A2, A3, A4), right: Z): (A1, A2, A3, A4, Z) =
          (left._1, left._2, left._3, left._4, right)
      }

    implicit def Zippable6[A1, A2, A3, A4, A5, Z]: Zippable.Out[(A1, A2, A3, A4, A5), Z, (A1, A2, A3, A4, A5, Z)] =
      new Zippable[(A1, A2, A3, A4, A5), Z] {
        type Out = (A1, A2, A3, A4, A5, Z)

        override def zip(left: (A1, A2, A3, A4, A5), right: Z): (A1, A2, A3, A4, A5, Z) =
          (left._1, left._2, left._3, left._4, left._5, right)
      }

    implicit def Zippable7[A1, A2, A3, A4, A5, A6, Z]
      : Zippable.Out[(A1, A2, A3, A4, A5, A6), Z, (A1, A2, A3, A4, A5, A6, Z)] =
      new Zippable[(A1, A2, A3, A4, A5, A6), Z] {
        type Out = (A1, A2, A3, A4, A5, A6, Z)

        override def zip(left: (A1, A2, A3, A4, A5, A6), right: Z): (A1, A2, A3, A4, A5, A6, Z) =
          (left._1, left._2, left._3, left._4, left._5, left._6, right)
      }
  }

  trait ImplicitsLowPriority {
    implicit def Zippable2[A, Z]: Zippable.Out[A, Z, (A, Z)] =
      new Zippable[A, Z] {
        type Out = (A, Z)

        override def zip(left: A, right: Z): Out = (left, right)
      }
  }
}
