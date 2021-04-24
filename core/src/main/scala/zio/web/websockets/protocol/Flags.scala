package zio.web.websockets.protocol

import zio.web.websockets.codec.{ BitChunk, FrameCodec }

final case class Flags(fin: Boolean, rsv1: Boolean = false, rsv2: Boolean = false, rsv3: Boolean = false)

object Flags {
  val last: Flags     = Flags(true)
  val continue: Flags = Flags(false)

  val codec: FrameCodec[Flags] =
    FrameCodec
      .bits(4)
      .imap[BitChunk, Flags](
        bits => Flags(bits.get(0), bits.get(1), bits.get(2), bits.get(3)),
        flags => BitChunk.bits(flags.fin, flags.rsv1, flags.rsv2, flags.rsv3)
      )
}
