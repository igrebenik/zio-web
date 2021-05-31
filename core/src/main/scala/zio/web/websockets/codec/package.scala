package zio.web.websockets

import zio.Chunk

package object codec {
  type Bits = Chunk[Boolean]

  object Bits {
    def fromByte(value: Byte): Bits = Chunk(value).asBits

    def fromShort(value: Short): Bits = Chunk((value << 8 & 0xFF00).toByte, (value & 0xFF).toByte).asBits

    def fromInt(value: Int): Bits =
      Chunk(
        (value >> 24 & 0xFF).toByte,
        (value >> 16 & 0xFF).toByte,
        (value >> 8 & 0xFF).toByte,
        (value & 0xFF).toByte
      ).asBits

    def fromLong(value: Long): Bits =
      Chunk(
        (value >> 56 & 0xFF).toByte,
        (value >> 48 & 0xFF).toByte,
        (value >> 40 & 0xFF).toByte,
        (value >> 32 & 0xFF).toByte,
        (value >> 24 & 0xFF).toByte,
        (value >> 16 & 0xFF).toByte,
        (value >> 8 & 0xFF).toByte,
        (value & 0xFF).toByte
      ).asBits

    implicit class BitsOps(val bits: Bits) extends AnyVal {
      def bytes: Chunk[Byte]     = ???
      def toInt: Option[Int]     = ???
      def toShort: Option[Short] = ???
    }
  }
}
