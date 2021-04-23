package zio.web.websockets.codec

import zio.Chunk

sealed private[websockets] trait FrameCodec[+A] { self =>
  import FrameCodec._

  def <*>[B](that: FrameCodec[B])(implicit z: Zippable[A, B]): FrameCodec[z.Out] =
    self.zip(that)

  def zip[B](that: FrameCodec[B])(implicit z: Zippable[A, B]): FrameCodec[z.Out] = ???

  def encode[A1 >: A](a: A1): Either[String, BitChunk] = ???

  def decode(bits: BitChunk): Either[String, A] = ???

  def imap[A1 >: A, B](f: A1 => B, g: B => A1): FrameCodec[B] = ???

  def map[B](f: A => B): FrameCodec[B] = ???

  def eimap[A1 >: A, B](f: A1 => Either[String, B], g: B => Either[String, A1]): FrameCodec[B] = ???
}

private[websockets] object FrameCodec {
  def apply[A](encode: A => BitChunk, decode: BitChunk => A): FrameCodec[A] = ???

  val bit: FrameCodec[BitChunk] = ???

  def bits(n: Int): FrameCodec[BitChunk] = ???

  val byte: FrameCodec[Byte] = ???

  def byte(bits: Int = 8): FrameCodec[BitChunk] = ???

  def bytes(length: Int): FrameCodec[Chunk[Byte]] = ???

  def bytes: FrameCodec[Chunk[Byte]] = ???

  val int: FrameCodec[BitChunk] = ???

  def string: FrameCodec[String] = ???

  def value[A]: FrameCodec[A] = ???

  val result = value[Int] <*> value[String] <*> value[Long] <*> bytes(3) <*> byte

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
