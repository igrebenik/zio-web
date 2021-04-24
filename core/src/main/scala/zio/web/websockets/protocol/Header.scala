package zio.web.websockets.protocol

import zio.web.websockets.codec.{ BitChunk, FrameCodec }

final case class Header(flags: Flags, opCode: OpCode, maskingKey: Option[Int], length: Int)

object Header {

  val maskingKeyCodec: FrameCodec[Option[Int]] =
    FrameCodec
      .bits(4 * 8)
      .imap[BitChunk, Option[Int]](
        ch => if (ch.isEmpty) None else Some(ch.toByteBuffer.getInt),
        optInt => optInt.fold(BitChunk.empty)(BitChunk.int(_))
      )

  val lengthCodec: FrameCodec[Int] =
    FrameCodec
      .bitsAtLeast(7)
      .eimap[BitChunk, Int](
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
    (Flags.codec <*> OpCode.codec <*> maskingKeyCodec <*> lengthCodec)
      .imap[(Flags, OpCode, Option[Int], Int), Header](
        {
          case (flags, opcode, maskingKey, length) =>
            Header(flags, opcode, maskingKey, length)
        },
        h => (h.flags, h.opCode, h.maskingKey, h.length)
      )

}
