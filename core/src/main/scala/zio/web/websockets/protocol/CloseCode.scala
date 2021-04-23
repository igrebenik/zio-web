package zio.web.websockets.protocol

import scala.annotation.switch

import zio.web.websockets.codec.{ BitChunk, FrameCodec }

sealed abstract class CloseCode(val code: Short) extends Product with Serializable

object CloseCode {
  case object NormalClosure       extends CloseCode(1000)
  case object GoingAway           extends CloseCode(1001)
  case object ProtocolError       extends CloseCode(1002)
  case object UnsupportedData     extends CloseCode(1003)
  case object NoStatusReceived    extends CloseCode(1005)
  case object AbnormalClosure     extends CloseCode(1006)
  case object InvalidPayloadData  extends CloseCode(1007)
  case object PolicyViolation     extends CloseCode(1008)
  case object MessageTooBig       extends CloseCode(1009)
  case object MandatoryExtension  extends CloseCode(1010)
  case object InternalServerError extends CloseCode(1011)
  case object TLSHandshake        extends CloseCode(1015)

  def fromShort(code: Short): Option[CloseCode] =
    (code: @switch) match {
      case 1000 => Some(NormalClosure)
      case 1001 => Some(GoingAway)
      case 1002 => Some(ProtocolError)
      case 1003 => Some(UnsupportedData)
      case 1005 => Some(NoStatusReceived)
      case 1006 => Some(AbnormalClosure)
      case 1007 => Some(InvalidPayloadData)
      case 1008 => Some(PolicyViolation)
      case 1009 => Some(MessageTooBig)
      case 1010 => Some(MandatoryExtension)
      case 1011 => Some(InternalServerError)
      case 1015 => Some(TLSHandshake)
      case _    => None
    }

  val codec: FrameCodec[CloseCode] =
    FrameCodec
      .bits(16)
      .eimap[BitChunk, CloseCode](
        ch =>
          fromShort(ch.toShort) match {
            case None       => Left(s"Invalid close code: ${ch.toShort}")
            case Some(code) => Right(code)
          },
        code => Right(BitChunk.short(code.code))
      )

}
