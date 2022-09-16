import cats.effect.{Async, IO, Sync}
import fs2._

def writeToSocket[F[_] : Async](chunk: Chunk[String]): F[Unit] =
  Async[F].async { callback =>
    println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
    callback(Right(()))
    Sync[F].delay(Some(Sync[F].delay(())))
  }

def writeToSocket0[F[_] : Async](chunk: Chunk[String]): IO[Unit] =
  IO(println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket"))


Stream((1 to 100).map(_.toString): _*)
  .chunkN(10)
  .covary[IO]
  .parEvalMapUnordered(10)(writeToSocket[IO])
  .compile
  .drain





