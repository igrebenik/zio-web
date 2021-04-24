package zio.web.websockets.codec

import java.nio.ByteBuffer

import zio.Chunk

// bit ordering: Big Endian / Little Endian ?
// padding: padRight, padLeft ?
// strict size control !!!
final private[websockets] case class BitChunk(
  bytes: Chunk[Byte],
  minBitIndex: Int,
  maxBitIndex: Int
) { self =>

  val length: Int =
    maxBitIndex - minBitIndex

  val isEmpty: Boolean =
    length == 0

  val nonEmpty: Boolean =
    !isEmpty

  def apply(n: Int): Boolean =
    (bytes(n >> 3) & (1 << (7 - (n & 7)))) != 0

  def get(n: Int): Boolean =
    apply(n)

  @throws[IndexOutOfBoundsException]
  def getByte(n: Int): Byte =
    bytes(n)

  def update(n: Int, bit: Boolean): BitChunk = ???

  def insert(n: Int, bit: Boolean): BitChunk = ???

  def set(n: Int): BitChunk =
    update(n, true)

  def clear(n: Int): BitChunk =
    update(n, false)

  def ++(that: BitChunk): BitChunk =
    BitChunk(
      self.bytes ++ that.bytes,
      self.minBitIndex,
      self.maxBitIndex + that.maxBitIndex
    )

  def :+(bit: Boolean): BitChunk =
    self ++ BitChunk.bit(bit)

  def +:(bit: Boolean): BitChunk =
    BitChunk.bit(bit) ++ self

  def drop(n: Int): BitChunk = {
    val index  = (minBitIndex + n).min(maxBitIndex)
    val toDrop = index >> 3
    val min    = index & 7
    val max    = maxBitIndex - index + min
    println(s"min=$min, max=$max")
    BitChunk(bytes.drop(toDrop), min, max)
  }

  def |(that: BitChunk): BitChunk = ???

  def &(that: BitChunk): BitChunk = ???

  def ^(that: BitChunk): BitChunk = ???

  def >>(n: Long): BitChunk = ???

  def <<(n: Long): BitChunk = ???

  def foreach[A](f: Boolean => A): Unit = {
    val minByteIndex    = (minBitIndex + 7) >> 3
    val maxByteIndex    = maxBitIndex >> 3
    val minFullBitIndex = (minByteIndex << 3).min(maxBitIndex)
    val maxFullBitIndex = (maxByteIndex << 3).max(minFullBitIndex)
    var i               = minBitIndex
    while (i < minFullBitIndex) {
      f(apply(i))
      i += 1
    }
    i = minByteIndex
    while (i < maxByteIndex) {
      val byte = bytes(i)
      f((byte & 128) != 0)
      f((byte & 64) != 0)
      f((byte & 32) != 0)
      f((byte & 16) != 0)
      f((byte & 8) != 0)
      f((byte & 4) != 0)
      f((byte & 2) != 0)
      f((byte & 1) != 0)
      i += 1
    }
    i = maxFullBitIndex
    while (i < maxBitIndex) {
      f(apply(i))
      i += 1
    }
  }

  def take(n: Int): BitChunk = {
    val index  = (minBitIndex + n).min(maxBitIndex)
    val toTake = (index + 7) >> 3
    BitChunk(bytes.take(toTake), minBitIndex, index)
  }

  def toArray[A1 >: Boolean](n: Int, dest: Array[A1]): Unit = {
    var i = n
    while (i < length) {
      dest(i + n) = apply(i)
      i += 1
    }
  }

  def toByteBuffer: ByteBuffer = ???

  def toByte: Byte = ???

  def toInt: Int = ???

  def toShort: Short = ???

  def toLong: Long = ???
}

private[websockets] object BitChunk {
  val empty: BitChunk = BitChunk(Chunk.empty, 0, 0)

  val oneLow: BitChunk = empty.insert(0, false)

  val oneHigh: BitChunk = empty.insert(0, true)

  def bit(high: Boolean): BitChunk = if (high) oneHigh else oneLow

  def bits(bit: Boolean, bits: Boolean*): BitChunk = ???

  def bits(bits: Chunk[Boolean]): BitChunk = ???

  def int(int: Int, size: Int = 32): BitChunk = ???

  def long(long: Long, size: Int = 64): BitChunk = ???

  def short(short: Short, size: Int = 16): BitChunk = ???

  def byte(byte: Byte, size: Int = 8): BitChunk = ???

  def bytes(bytes: Chunk[Byte]): BitChunk = ???

  def bytes(byte: Byte, bytes: Byte*): BitChunk = ???

  def fromByteBuffer(buffer: ByteBuffer): BitChunk = ???
}
