package zio.web.websockets.codec

sealed trait BitsOrdering

object BitsOrdering {
  case object BigEndian    extends BitsOrdering
  case object LittleEndian extends BitsOrdering
}
