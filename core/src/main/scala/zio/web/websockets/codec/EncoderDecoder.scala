package zio.web.websockets.codec

sealed trait Encoder[-A] { self =>

  // contramap...
  final def transform[B](f: B => A): Encoder[B] = ???

  final def transformOrFail[B](f: B => Either[String, A]): Encoder[B] = ???

  def length: BitsLength

  def encode(a: A): Either[String, BitChunk]

}

object Encoder {
  def apply[A](encode: A => Either[String, BitChunk], length: BitsLength): Encoder[A] = ???

  val int: Encoder[Int]   = ???
  val char: Encoder[Char] = int.transform(_.toInt)
}

// ===================

sealed trait Decoder[+A] {

  // map
  final def transform[B](f: A => B): Decoder[B] = ???

  // flatMap
  final def andThen[B](f: A => Decoder[B]): Decoder[B] = ???

  def length: BitsLength

  def decode(bits: BitChunk): Either[String, A]
}

object Decoder {
  def apply[A](decode: BitChunk => Either[String, A]): Decoder[A] = ???
}

// ===================
// It would super nice to work with Schema[A] rather than with type A directly
// It will be possible to derive Codec[A] out of Schema[A] !!!
sealed trait Codec[A] extends Encoder[A] with Decoder[A] { self =>

  final def transformBoth[B](f: A => B, g: B => A): Codec[B] =
    new Codec[B] {
      val length: BitsLength = self.length

      override def encode(b: B): Either[String, BitChunk] =
        self.encode(g(b))

      override def decode(bits: BitChunk): Either[String, B] =
        self.decode(bits).map(f)
    }

  final def transformBothOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Codec[B] =
    new Codec[B] {
      val length: BitsLength = self.length

      override def encode(b: B): Either[String, BitChunk] =
        g(b).flatMap(self.encode)

      override def decode(bits: BitChunk): Either[String, B] =
        self.decode(bits).flatMap(f)
    }

  final def zip[B](that: Codec[B])(implicit z: Zippable[A, B]): Codec[z.Out] =
    new Codec[z.Out] {
      val length: BitsLength = self.length.atLeast

      override def encode(t: z.Out): Either[String, BitChunk] =
        for {
          l <- self.encode(z.left(t))
          r <- that.encode(z.right(t))
        } yield l ++ r

      override def decode(bits: BitChunk): Either[String, z.Out] =
        // (for {
        //   l <- self
        //   r <- that
        // } yield z.zip(l, r)).decode(bits)
        self.andThen(l => that.transform(r => z.zip(l, r))).decode(bits)
    }

  final def bigEndian: Codec[A] = ???

  final def littleEndian: Codec[A] = ???
}

object Codec {

  def apply[A](encode: A => Either[String, BitChunk], decode: BitChunk => Either[String, A]): Codec[A] = ???

  def apply[A](encoder: Encoder[A], decoder: Decoder[A]): Codec[A] =
    apply(a => encoder.encode(a), bits => decoder.decode(bits))

  final case class IntCodec(len: Int, signed: Boolean, ordering: BitsOrdering = BitsOrdering.BigEndian)
      extends Codec[Int] {

    override val length: BitsLength                          = BitsLength.exact(len)
    override def encode(int: Int): Either[String, BitChunk]  = ???
    override def decode(bits: BitChunk): Either[String, Int] = ???
  }

  val int4: Codec[Int]   = IntCodec(4, signed = true)
  val int8: Codec[Int]   = IntCodec(8, signed = true)
  val int16: Codec[Int]  = IntCodec(16, signed = true)
  val int32: Codec[Int]  = IntCodec(32, signed = true)
  val uint4: Codec[Int]  = IntCodec(4, signed = false)
  val uint8: Codec[Int]  = IntCodec(8, signed = false)
  val uint16: Codec[Int] = IntCodec(16, signed = false)
  val uint32: Codec[Int] = IntCodec(32, signed = false)
  val int4l: Codec[Int]  = IntCodec(4, signed = true, ordering = BitsOrdering.LittleEndian)
}
