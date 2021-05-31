package zio.web.websockets.protocol

import zio.Chunk
import zio.web.websockets.codec.Codec

final case class Flags(fin: Boolean, rsv1: Boolean = false, rsv2: Boolean = false, rsv3: Boolean = false)

object Flags {
  val last: Flags     = Flags(true)
  val continue: Flags = Flags(false)

  val codec: Codec[Flags] =
    Codec
      .bits(4)
      .transform(
        bits => Flags(bits(0), bits(1), bits(2), bits(3)),
        flags => Chunk(flags.fin, flags.rsv1, flags.rsv2, flags.rsv3)
      )
}
