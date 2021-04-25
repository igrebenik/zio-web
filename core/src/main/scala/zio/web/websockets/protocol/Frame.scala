package zio.web.websockets.protocol

import zio.Chunk

import zio.web.websockets.codec.FrameCodec

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

    val codec: FrameCodec[Frame.Text] =
      (Header.codec <*> FrameCodec.bytes)
        .imap[(Header, Chunk[Byte]), Frame.Text](
          {
            case (header, payload) =>
              Frame.Text(new String(payload.toArray, "UTF-8"), header.flags.fin)
          },
          frame => (frame.header, Chunk.fromArray(frame.payload.getBytes("UTF-8")))
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

    val codec: FrameCodec[Frame.Binary] =
      (Header.codec <*> FrameCodec.bytes)
        .imap[(Header, Chunk[Byte]), Frame.Binary](
          {
            case (header, payload) =>
              Frame.Binary(payload, header.flags.fin)
          },
          frame => (frame.header, frame.paylaod)
        )
  }

  final case class Ping() extends Frame {

    val header: Header =
      Header(Flags.last, OpCode.Ping, None, 0)

  }

  object Ping {

    val codec: FrameCodec[Frame.Ping] =
      Header.codec.imap[Header, Frame.Ping](_ => Frame.Ping(), ping => ping.header)

  }

  final case class Pong() extends Frame {

    val header: Header =
      Header(Flags.last, OpCode.Pong, None, 0)

  }

  object Pong {

    val codec: FrameCodec[Frame.Pong] =
      Header.codec.imap[Header, Frame.Pong](_ => Frame.Pong(), pong => pong.header)

  }

  final case class Close(code: CloseCode, reason: String) extends Frame {

    val header: Header =
      Header(Flags.last, OpCode.Close, None, reason.getBytes().length + 2)

  }

  object Close {

    val code: FrameCodec[Frame.Close] =
      (Header.codec <*> CloseCode.codec <*> FrameCodec.string)
        .imap[(Header, CloseCode, String), Frame.Close](
          {
            case (_, code, reason) =>
              Frame.Close(code, reason)
          },
          code => (code.header, code.code, code.reason)
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

    val code: FrameCodec[Frame.Continuation] =
      (Header.codec <*> FrameCodec.bytes)
        .imap[(Header, Chunk[Byte]), Frame.Continuation](
          {
            case (header, payload) =>
              Frame.Continuation(payload, header.flags.fin)
          },
          frame => (frame.header, frame.payload)
        )

  }

}
