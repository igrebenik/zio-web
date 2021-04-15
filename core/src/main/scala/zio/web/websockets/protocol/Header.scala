package zio.web.websockets.protocol

final case class Header(flags: Flags, opCode: Opcode, maskingKey: Option[Int], length: Int)
