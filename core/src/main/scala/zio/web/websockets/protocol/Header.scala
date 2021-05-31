package zio.web.websockets.protocol

import zio.web.websockets.codec._
import zio.web.websockets.codec.Bits._

final case class Header(flags: Flags, opCode: OpCode, maskingKey: Option[Int], length: Int)

object Header {

  val maskingKeyCodec: Codec[Option[Int]] = Codec.int.optional

  val lengthCodec: Codec[Int] =
    Codec
      .bitRange(7, 71)
      .transformOrFail(
        bits => {
          val len = bits.take(7).bytes(0)

          if (len < 126) Right(len.toInt)
          else if (len == 126 | len == 127) {
            bits.drop(7).toInt match {
              case None      => Left("Couldn't get Int value without loss of precision")
              case Some(int) => Right(int)
            }
          } else Left("Unsupported payload length")
        },
        len =>
          if (len < 126) Right(Bits.fromByte((len & 0xFF).toByte))
          else if (len < 65536) Right(Bits.fromByte(0x7E.toByte) ++ Bits.fromShort(len.toShort))
          else if (len <= Int.MaxValue) Right(Bits.fromByte(0x7F.toByte) ++ Bits.fromLong(len.toLong))
          else Left("the length of the paylod is greater than Int.MaxValue")
      )

  val codec: Codec[Header] =
    (Flags.codec ~ OpCode.codec ~ Codec.boolean ~ lengthCodec ~ maskingKeyCodec)
      .transformOrFail(
        {
          case (flags, opCode, true, length, Some(maskingKey)) => Right(Header(flags, opCode, Some(maskingKey), length))
          case (flags, opCode, false, length, None)            => Right(Header(flags, opCode, None, length))
          case _                                               => Left("masking key has invalid value")
        },
        header => Right((header.flags, header.opCode, header.maskingKey.isDefined, header.length, header.maskingKey))
      )
}
