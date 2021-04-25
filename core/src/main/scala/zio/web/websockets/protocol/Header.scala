package zio.web.websockets.protocol

import zio.web.websockets.codec.{ BitChunk, FrameCodec }

final case class Header(flags: Flags, opCode: OpCode, maskingKey: Option[Int], length: Int)

object Header {

  val maskingKeyCodec: FrameCodec[Int] =
    FrameCodec
      .bits(4 * 8)
      .imap[BitChunk, Int](
        ch => ch.toByteBuffer.getInt,
        int => BitChunk.int(int)
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

  //TODO: maskingKey needs to be optional (Option[Int])
  val codec: FrameCodec[Header] =
    (Flags.codec <*> OpCode.codec <*> FrameCodec.bit <*> lengthCodec <*> maskingKeyCodec)
      .imap[(Flags, OpCode, BitChunk, Int, Int), Header](
        {
          case (flags, opcode, maskBit, length, maskingKey) =>
            if (maskBit.get(0))
              Header(flags, opcode, Some(maskingKey), length)
            else
              Header(flags, opcode, None, length)
        },
        h =>
          h.maskingKey match {
            case None      => (h.flags, h.opCode, BitChunk.oneLow, h.length, 0)
            case Some(key) => (h.flags, h.opCode, BitChunk.oneHigh, h.length, key)
          }
      )

}
