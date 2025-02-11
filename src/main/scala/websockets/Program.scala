package websockets

import cats.effect.IOApp
import cats.effect.IO
import Server.server
import fs2.concurrent.Topic
import cats.effect.kernel.Ref
import cats.effect.std.Queue
import fs2.Stream
import websockets.SuccessfulRegistration.KeepAlive
import websockets.Util.ChatState

import concurrent.duration.DurationInt

object Program extends IOApp.Simple {
  private def program: IO[Unit] = {
    for {
      q        <- Queue.unbounded[IO, OutputMessage]
      t        <- Topic[IO, OutputMessage]
      cs       <- Ref.of[IO, ChatState](ChatState(Map.empty, Map.empty))
      protocol <- IO(Protocol.make[IO](cs))
      im       <- IO(InputMessage.make[IO](protocol))
      s <- Stream(
        Stream.fromQueueUnterminated(q).through(t.publish),
        Stream
          .awakeEvery[IO](30.seconds)
          .map(_ => KeepAlive)
          .through(t.publish),
        Stream.eval(server[IO](q, t, im, protocol, cs))
      ).parJoinUnbounded.compile.drain
    } yield s
  }

  override def run: IO[Unit] = program
}
