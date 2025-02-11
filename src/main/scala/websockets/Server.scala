package websockets

import fs2.concurrent.Topic
import org.http4s.websocket.WebSocketFrame
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.*
import cats.effect.kernel.Async
import cats.syntax.all.*
import fs2.io.net.Network
import fs2.io.file.Files
import websockets.Util.ChatState

object Server {
  def server[F[_]: Async: Files: Network](
      q: Queue[F, OutputMessage],
      t: Topic[F, OutputMessage],
      im: InputMessage[F],
      protocol: Protocol[F],
      cs: Ref[F, ChatState]
  ): F[Unit] = {
    val host = host"0.0.0.0"
    val port = port"8080"

    EmberServerBuilder
      .default[F]
      .withHost(host)
      .withPort(port)
      .withHttpWebSocketApp(wsb => new Routes().service(wsb, q, t, im, protocol, cs))
      .build
      .useForever
      .void
  }
}
