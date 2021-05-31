package zio.web.websockets.protocol

import zio.web.websockets.codec.Bits._
import zio.web.websockets.codec._

import scala.annotation.switch

sealed abstract class OpCode(val code: Byte)

object OpCode {
  case object Continuation extends OpCode(0x00)
  case object Text         extends OpCode(0x01)
  case object Binary       extends OpCode(0x02)
  case object Close        extends OpCode(0x08)
  case object Ping         extends OpCode(0x09)
  case object Pong         extends OpCode(0x0A)

  def fromByte(byte: Byte): Option[OpCode] =
    (byte: @switch) match {
      case 0x00 => Some(Continuation)
      case 0x01 => Some(Text)
      case 0x02 => Some(Binary)
      case 0x08 => Some(Close)
      case 0x09 => Some(Ping)
      case 0x0A => Some(Pong)
      case _    => None
    }

  val codec: Codec[OpCode] =
    Codec
      .bits(4)
      .transformOrFail(
        bits =>
          fromByte(bits.bytes(0)) match {
            case None         => Left(s"the valid opcode is not found")
            case Some(opcode) => Right(opcode)
          },
        opcode => Right(Bits.fromByte(opcode.code))
      )
}
