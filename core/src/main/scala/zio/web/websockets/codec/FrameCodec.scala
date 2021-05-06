package zio.web.websockets.codec

import zio.Chunk

sealed private[websockets] trait FrameCodec[A] { self =>
  import FrameCodec._

  def <*>[B](that: FrameCodec[B])(implicit z: Zippable[A, B]): FrameCodec[z.Out] =
    self.zip(that)

  def zip[B](that: FrameCodec[B])(implicit z: Zippable[A, B]): FrameCodec[z.Out] =
    Tuple[A, B, z.Out](self, that)

  def optional: FrameCodec[Option[A]] =
    FrameCodec.Optional(self)

  def transform[B](f: A => B, g: B => A): FrameCodec[B] =
    transformOrFail(a => Right(f(a)), b => Right(g(b)))

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): FrameCodec[B] =
    FrameCodec.TransformOrFail(self, f, g)

  def encode(a: A): Either[String, BitChunk] = ???

  def decode(bits: BitChunk): Either[String, A] = ???
}

private[websockets] object FrameCodec {

  final private case class TransformOrFail[A, B](
    codec: FrameCodec[A],
    f: A => Either[String, B],
    g: B => Either[String, A]
  ) extends FrameCodec[B]

  final private case class Optional[A](codec: FrameCodec[A]) extends FrameCodec[Option[A]]

  final private case class Tuple[A, B, Z](left: FrameCodec[A], right: FrameCodec[B]) extends FrameCodec[Z]

  final private case class Codec[A](encode: A => BitChunk, decode: BitChunk => A) extends FrameCodec[A]

  def apply[A](encode: A => BitChunk, decode: BitChunk => A): FrameCodec[A] =
    Codec(encode, decode)

  val unit: FrameCodec[Unit] =
    FrameCodec(_ => BitChunk.empty, _ => ())

  val bit: FrameCodec[BitChunk] =
    FrameCodec(identity, identity)

  val byte: FrameCodec[Byte] =
    FrameCodec(byte => BitChunk.bytes(byte), chunk => chunk.toByte)

  val bytes: FrameCodec[Chunk[Byte]] =
    FrameCodec(bs => BitChunk.bytes(bs), chunk => chunk.bytes)

  val int: FrameCodec[Int] =
    FrameCodec(int => BitChunk.int(int), chunk => chunk.toInt)

  val string: FrameCodec[String] =
    bytes.transform(
      bs => new String(bs.toArray, "UTF-8"),
      s => Chunk.fromArray(s.getBytes("UTF-8"))
    )

  def bits(length: Int): FrameCodec[BitChunk] = ???

  def bitsAtLeast(n: Int): FrameCodec[BitChunk] = ???

}
