package exa.scs

import cats.effect.kernel.Deferred
import cats.effect.std.Supervisor
import cats.effect.testkit.TestControl
import cats.effect.{Async, ExitCode, IO, IOApp, LiftIO, Ref, Sync}
import cats.implicits.{catsSyntaxEitherId, toFlatMapOps, toFunctorOps}
import fs2._
import fs2.concurrent.SignallingRef
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}

import scala.concurrent.duration.{DurationDouble, DurationInt}
import cats.effect.std.Random
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.effect.PropF
import shop.domain.order.PaymentId

import java.util.UUID


/*TestDoku
Für IOTests:
Worksheet
Noch Besser IOApp als Worksheet: nur ausgewähltes läuft, kein Rebuild Projekt nötig, damit imports in Worksheet funktionieren
Noch Besser CatsEffectSuite als IOApp: nur ausgewählten "test" laufen lassen (auch generell ohne IO)
    - Achtung: als MUnit single test über gutter: notwendig   "class .... extends CatsEffectSuite" UND toplevel einziges Element im File!!
 */
//object StreamTry extends IOApp {
class StreamTry extends CatsEffectSuite with ScalaCheckEffectSuite {
  def runStream0(stream : Stream[IO,_] ) =
    stream.compile.drain .map { r =>  assertEquals(true, true) } //println erscheint

  def runStream(stream : Stream[IO,_] ) =
    TestControl.executeEmbed(stream.compile.drain).map { r => //println erscheint
      assertEquals(true, true)
    }

  def testControled[A](fa : IO[A]) = TestControl.executeEmbed(fa)

  def supervised[A](fa : IO[A] ) =
    Supervisor[IO].use {   sp =>    //println erscheint  nicht !!!
      sp.supervise(fa).void
    }

  def idGen[A](f: UUID => A): Gen[A] =
    Gen.uuid.map(f)

/*
*/
  implicit lazy val paymentIdGen: Gen[PaymentId] =
    idGen(PaymentId.apply)

  implicit def arbA[A](implicit GA: Gen[A]): Arbitrary[A] = Arbitrary { GA }

  test("first PropF test") {
    PropF.forAllF { (x: PaymentId) =>
      IO(x).start.flatMap(_.join).map(res => {println(s"$x - $res"); () } )
    }
  }



  //asynchron
  def writeToSocketA[F[_]: Async](chunk: Chunk[String]): F[String] =
    Async[F].async { callback =>
      println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
      callback(Right("Hi"))
      Sync[F].delay(Some(Sync[F].delay(())))
    }

  //synchron
  def writeToSocketS[F[_]: Sync](chunk: Chunk[String]): F[Unit] =
    Sync[F].delay(println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket"))

  //synchron mit Exception
  def writeToSocketSE[F[_]: Sync](chunk: Chunk[String]): F[Int] =
    Random.scalaUtilRandom(Sync[F]).flatMap { r =>
      r.nextInt.map { rn =>
        println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
        if (rn % 2 == 0) {
          throw new RuntimeException(s"RuntimeException bei  $chunk")
        }
        println(s"Saved:  $chunk mit id $rn")
        rn
      }
    }


  val stpwrite = Stream((1 to 100).map(_.toString): _*)
    .chunkN(10)
    .covary[IO]

  val stpwriteA = stpwrite
    .parEvalMapUnordered(10)(writeToSocketA[IO])

  val stpwriteSE = stpwrite
    .parEvalMapUnordered(10)(writeToSocketSE[IO])
    //   .handleErrorWith(error => Stream.eval(IO.println(s"Error: $error")))
    .attempt
    .evalMap {
      case Left(error) => IO.println(s"Nach attempt: Left Error: $error")
      case Right(id) => IO.println(s"Nach attempt: Right: $id")
    }


  test("run  Stream parEvalMapUnordered  chunkN ") {
    runStream(stpwriteA)
  }

  test("2run  Stream parEvalMapUnordered  chunkN ") {
    runStream(stpwriteSE)
  }


  //https://www.beyondthelines.net/programming/streaming-patterns-with-fs2/
  def writeToDatabase[F[_]: Async](chunk: Chunk[Int]): F[Unit] =
    Async[F].async { callback =>
      println(s"Writing batch of $chunk to database by ${Thread.currentThread().getName}")
      callback(Right(()))
      Sync[F].delay(Some(Sync[F].delay(())))
    }

  test("run  Stream chunkN(10) ") {
    runStream(
      Stream.emits(1 to 1000)
      .chunkN(10)
      .covary[IO]
      .parEvalMap(10)(writeToDatabase[IO])
    )
  }

  def pipe[F[_] : Sync](name: String): Stream[F, Int] => Stream[F, Int] = {
    _.evalTap { index =>
      Sync[F].delay(
        println(s"Stage $name processing $index by ${Thread.currentThread().getName}")
      )
    }
    //todo!!!
  }

  def pipeAsync[F[_]: Async : Sync](name: String): Stream[F, Int] => Stream[F, Int] =
    _.evalTap { index =>
      Async[F].async { (cb : (Either[Throwable, Int] => Unit) ) =>
      IO(println(s"Stage $name processing $index by ${Thread.currentThread().getName}"))
        Sync[F].delay(Some(Sync[F].delay(())))
      }
    }

  test("run  Stream pipe ABC ") {
  Stream.emits(1 to 1000)
    .covary[IO]
    .through(pipe("A"))
    .through(pipe("B"))
    .through(pipe("C"))
  }


  val stinterrupt = Stream
    .eval(Deferred[IO, Either[Throwable, Unit]])
    .evalMap( switch => Random.scalaUtilRandom[IO].map(r => (switch, r)) )
    .flatMap {  case (switch, rand) =>
      Stream
        .repeatEval(rand.betweenInt(0,5))
        .metered(1.second)
        .evalTap(IO.println)
        .evalTap { n =>
          if(n == 4) switch.complete(().asRight).void  else IO.unit
        }
        .interruptWhen(switch)
        .onFinalize(IO.println("Interrupted!"))
    }


  test("run  Stream interruptWhen  Deferred complete  ") {
    runStream(stinterrupt)
  }


  val stpause =  Stream   //qcash test!??
    .eval(SignallingRef[IO, Boolean](false))
    .flatMap { signal =>
      val src =
        Stream
          .repeatEval(IO.println("ping"))
          .pauseWhen(signal)
          .metered(1.second)
      val pause =
        Stream
          .sleep[IO](3.seconds)
          .evalTap(_ => IO.println("ႍ>> Pausing stream ႍ<<"))
          .evalTap(_ => signal.set(true))
      val resume =
        Stream
          .sleep[IO](7.seconds)
          .evalTap(_ => IO.println("ႍ>> Resuming stream ႍ<<"))
          .evalTap(_ => signal.set(false))
      Stream(src, pause, resume).parJoinUnbounded
    }
    .interruptAfter(10.seconds)
    .onFinalize(IO.println("pong"))

  test("run  Stream pauseWhen SignallingRef  ") {
    runStream0(stpause)
  }


  val data: Stream[IO,Int] = {
    Stream.range(1, 10).covary[IO].metered(1.seconds)
  }
  val stSig = Stream.eval(fs2.concurrent.SignallingRef[IO,Int](0)).flatMap(s =>
    Stream(s).concurrently(data.evalMap(s.set))).flatMap(_.discrete).debug().takeWhile(_ < 7, true)
  // .compile.last.unsafeRunSync()

  test("run SignallingRef discrete stream") {
    runStream(stSig)
  }

  def slprint(i:Any, rand: Random[IO[*]]) = rand.betweenInt(100, 3000).flatMap(d => IO.sleep(d.milliseconds)) *> IO(println(i))

  val parEvalSt = Stream(1,2,3,4,6,7,8,9).covary[IO].parEvalMapUnordered(2)(i => Random.scalaUtilRandom[IO].flatMap(r =>  slprint(i, r)) )
  test("run parEvalMapUnordered IO") {
    runStream0(parEvalSt)
  }
  test("run parEvalMapUnordered executeEmbed") {
    runStream(parEvalSt)
  }

  test("run parEvalMapUnordered supervised") {
    supervised(parEvalSt.compile.drain)
  }


  test("run slprint") {
  //  supervised(
      for {
        rand <- Random.scalaUtilRandom[IO]
        eng <- slprint ("Hello", rand).foreverM.start
        fr <- slprint ("Bonjour", rand).foreverM.start
        sp <- slprint ("Hola", rand).foreverM.start

        _ <- IO.sleep(15.seconds)
        _ <- eng.cancel >> fr.cancel >> sp.cancel

      } yield ((eng, fr, sp))
 //   )
  }








}







