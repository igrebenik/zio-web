package zio.web.websockets.protocol

import zio.web.websockets.codec.{ BitChunk, FrameCodec }

final case class Header(flags: Flags, opCode: OpCode, maskingKey: Option[Int], length: Int)

object Header {

  val maskingKeyCodec: FrameCodec[Option[Int]] =
    FrameCodec
      .bits(4 * 8)
      .transform[Int](
        ch => ch.toByteBuffer.getInt,
        int => BitChunk.int(int)
      )
      .optional

  val lengthCodec: FrameCodec[Int] =
    FrameCodec
      .bitsAtLeast(7)
      .transformOrFail[Int](
        ch => {
          val len = ch.take(7).toByte

          if (len < 126) Right(len.toInt)
          else if (len == 126 | len == 127) Right(ch.drop(7).toInt)
          else Left("Unknown payload length")
        },
        len =>
          if (len < 126) Right(BitChunk.byte((len & 0xFF).toByte, size = 7))
          else if (len < 65536) Right(BitChunk.byte(0x7E, size = 7) ++ BitChunk.short(len.toShort))
          else if (len <= Int.MaxValue) Right(BitChunk.byte(0x7F, size = 7) ++ BitChunk.long(len.toLong))
          else Left("The length of a payload is greater than Int.MaxValue!!!")
      )

  val codec: FrameCodec[Header] =
    (Flags.codec <*> OpCode.codec <*> FrameCodec.bit <*> lengthCodec <*> maskingKeyCodec)
      .transform(
        {
          case (flags, opcode, maskBit, length, maskingKey) =>
            if (maskBit.get(0) && maskingKey.isDefined)
              Header(flags, opcode, maskingKey, length)
            else
              Header(flags, opcode, None, length)
        },
        h =>
          h.maskingKey match {
            case None          => (h.flags, h.opCode, BitChunk.oneLow, h.length, None)
            case key @ Some(_) => (h.flags, h.opCode, BitChunk.oneHigh, h.length, key)
          }
      )

}
