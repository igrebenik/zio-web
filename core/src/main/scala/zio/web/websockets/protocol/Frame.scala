package zio.web.websockets.protocol

import java.nio.charset.{ StandardCharsets => Charsets }

import zio.Chunk

import zio.web.websockets.codec.Codec
import zio.web.websockets.codec.Bits._

sealed trait Frame {
  def header: Header
}

object Frame {

  final case class Text(payload: String, last: Boolean = true) extends Frame {

    val header: Header =
      Header(
        if (last) Flags.last else Flags.continue,
        OpCode.Text,
        None,
        payload.length
      )
  }

  object Text {

    val codec: Codec[Frame.Text] =
      Header.codec
        .sizeBytes(_.length)
        .transform(
          { case (header, payload) => Frame.Text(new String(payload.bytes.toArray, Charsets.UTF_8), header.flags.fin) },
          frame => (frame.header, Chunk.fromArray(frame.payload.getBytes(Charsets.UTF_8)).asBits)
        )
  }

  final case class Binary(paylaod: Chunk[Byte], last: Boolean = true) extends Frame {

    val header: Header =
      Header(
        if (last) Flags.last else Flags.continue,
        OpCode.Binary,
        None,
        paylaod.length
      )
  }

  object Binary {

    val codec: Codec[Frame.Binary] =
      Header.codec
        .sizeBytes(_.length)
        .transform(
          { case (header, payload) => Frame.Binary(payload.bytes, header.flags.fin) },
          frame => (frame.header, frame.paylaod.asBits)
        )
  }

  final case class Ping() extends Frame {
    val header: Header = Header(Flags.last, OpCode.Ping, None, 0)
  }

  object Ping {
    val codec: Codec[Frame.Ping] = Header.codec.transform(_ => Frame.Ping(), _.header)
  }

  final case class Pong() extends Frame {
    val header: Header = Header(Flags.last, OpCode.Pong, None, 0)
  }

  object Pong {
    val codec: Codec[Frame.Pong] = Header.codec.transform(_ => Frame.Pong(), _.header)
  }

  final case class Close(code: CloseCode, reason: String) extends Frame {
    val header: Header = Header(Flags.last, OpCode.Close, None, reason.getBytes().length + 2)
  }

  object Close {

    val codec: Codec[Frame.Close] =
      (Header.codec ~ CloseCode.codec)
        .sizeBytes(_._1.length)
        .transform(
          { case ((_, code), reason) => Frame.Close(code, new String(reason.bytes.toArray, Charsets.UTF_8)) },
          frame => ((frame.header, frame.code), Chunk.fromArray(frame.reason.getBytes(Charsets.UTF_8)).asBits)
        )
  }

  final case class Continuation(payload: Chunk[Byte], last: Boolean = false) extends Frame {

    val header: Header =
      Header(
        if (last) Flags.last else Flags.continue,
        OpCode.Continuation,
        None,
        payload.length
      )
  }

  object Continuation {

    val codec: Codec[Frame.Continuation] =
      Header.codec
        .sizeBytes(_.length)
        .transform(
          { case (header, payload) => Frame.Continuation(payload.bytes, header.flags.fin) },
          frame => (frame.header, frame.payload.asBits)
        )
  }

}
