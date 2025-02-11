package websockets

import cats.data.Validated
import cats.syntax.all.*

object Util {
  private def validateItem[F](
      value: String,
      userOrRoom: F,
      name: String
  ): Validated[String, F] = {
    Validated.cond(
      value.length >= 2 && value.length <= 10,
      userOrRoom,
      s"$name must be between 2 and 10 characters"
    )
  }

  case class User(name: String)

  object User {
    def apply(name: String): Validated[String, User] =
      validateItem(name, new User(name), "User name")
  }

  case class Room(room: String)

  object Room {
    def apply(room: String): Validated[String, Room] =
      validateItem(room, new Room(room), "Room")
  }

  case class ChatState(
      userRooms: Map[User, Room],
      roomMembers: Map[Room, Set[User]]
  )

  object ChatState {
    def apply(userRooms: Map[User, Room], roomMembers: Map[Room, Set[User]]) =
      new ChatState(userRooms, roomMembers)
  }

  object Constants {
    val RegisterMessage: String =
      """|Register your username with the following command:
         |/name <username>""".stripMargin

    val HelpInfoText =
      """Commands:
        | /help             - Show this text
        | /room             - Change to default/entry room
        | /room <room name> - Change to specified room
        | /rooms            - List all rooms
        | /members          - List members in current room
          """.stripMargin
  }
}
