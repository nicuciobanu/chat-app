package websockets

import websockets.Util.*
import websockets.Util.Constants.RegisterMessage

sealed trait OutputMessage

case class Register(user: Option[User], message: String = RegisterMessage) extends OutputMessage

case class ParsingError(user: Option[User], message: String) extends OutputMessage

case class SuccessfulRegistration(
    user: User,
    message: String
) extends OutputMessage

object SuccessfulRegistration {
  def apply(user: User): SuccessfulRegistration = {
    user match {
      case User(name) =>
        new SuccessfulRegistration(user = user, message = s"$name entered the chat")
    }
  }

  case class UnsupportedCommand(
      user: Option[User],
      message: String = ""
  ) extends OutputMessage

  case object KeepAlive extends OutputMessage

  case object DiscardMessage extends OutputMessage

  case class SendToUser(user: User, message: String) extends OutputMessage {
    def forUser(targetUser: User): Boolean = targetUser == user
  }

  case class ChatMsg(from: User, to: User, message: String) extends OutputMessage {
    def forUser(targetUser: User): Boolean = targetUser == to
  }
}
