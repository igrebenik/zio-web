package zio.web.websockets.protocol

/**
 * Defines the type of payload data
 */
sealed abstract class OpCode(val code: Byte)

object OpCode {
  case object Continuation extends OpCode(0x00)
  case object Text         extends Opcode(0x01)
  case object Binary       extends OpCode(0x02)
  case object Close        extends OpCode(0x08)
  case object Ping         extends OpCode(0x09)
  case object Pong         extends Opcode(0x0A)
}
