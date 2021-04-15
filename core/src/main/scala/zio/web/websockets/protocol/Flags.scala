package zio.web.websockets.protocol

final case class Flags(fin: Boolean, rsv1: Boolean = false, rsv2: Boolean = false, rsv3: Boolean = false)

object Flags {
  val last: Flags     = Flags(true)
  val continue: Flags = Flags(false)
}
