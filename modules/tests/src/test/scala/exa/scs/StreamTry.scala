package exa.scs

import cats.MonadThrow
import cats.effect.kernel.Deferred
import cats.effect.std.Supervisor
import cats.effect.testkit.TestControl
import cats.effect.{Async, ExitCode, IO, IOApp, LiftIO, Ref, Sync}
import cats.implicits.{catsSyntaxEitherId, catsSyntaxFlatMapOps, toFlatMapOps, toFunctorOps}
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
  def writeToSocketAsync[F[_]: Async](chunk: Chunk[String]): F[String] =
    Async[F].async { callback =>
      println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
      callback(Right("Hi"))
      Sync[F].delay(Some(Sync[F].delay(())))
    }

  //synchron
  def writeToSocketSync[F[_]: Sync](chunk: Chunk[String]): F[Unit] =
    Sync[F].delay(println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket"))

  //synchron mit Exception
  def writeToSocketSyncEx[F[_]: Sync : MonadThrow](chunk: Chunk[String], handleEx: Boolean)  = {
      val fa = Random.scalaUtilRandom(Sync[F]).flatMap { r =>
      r.nextInt.map { rn =>
        println(s"[thread: ${Thread.currentThread().getName}] :: Writing $chunk to socket")
        if (rn % 2 == 0) {
          throw new RuntimeException(s"RuntimeException bei  $chunk")
        }
        println(s"Saved:  $chunk mit id $rn")
        rn
      }
    }
    if(handleEx)  MonadThrow[F].handleErrorWith(fa)(e => Sync[F].delay(println(s"Exception $e")) >> Sync[F].delay(43) )
    else fa
  }


  val sourceStream = Stream((1 to 100).map(_.toString): _*)
    .chunkN(10)
    .covary[IO]

  val writeStreamAsync = sourceStream
    .parEvalMapUnordered(10)(writeToSocketAsync[IO])

  /*
  bei Exception in Stream-Werten: Stream wird abgebrochen -> jedoch noch handleErrorWith, attempt  etc. möglich
  falls Stream weiterlaufen soll: in Effekt zur Erzeugung der Stream-Werte: : MonadThrow und handleErrorWith oder attempt...
   */
  val writeStreamAsyncEx = sourceStream
    .parEvalMapUnordered(10)(writeToSocketSyncEx(_, false)(Sync[IO], MonadThrow[IO]))
    //   .handleErrorWith(error => Stream.eval(IO.println(s"Error: $error")))
    .attempt.evalMap {
      case Left(error) => IO.println(s"Nach attempt: Left Error: $error")
      case Right(id) => IO.println(s"Nach attempt: Right: $id")
    }


  /*
    vgl. https://blog.rockthejvm.com/fs2/
  */
  case class Actor(id: Int, firstName: String, lastName: String)

  val tomHolland = Actor(0, "Henry", "Cavill")
  val henryCavil = tomHolland
  val galGodot = Actor(1, "Gal", "Godot")
  val ezraMiller = Actor(2, "Ezra", "Miller")
  val benFisher = Actor(3, "Ben", "Fisher")
  val rayHardy = Actor(4, "Ray", "Hardy")
  val tomHollandStream: Stream[Pure, Actor] = Stream.emit(tomHolland)
  val spiderMen: Stream[Pure, Actor] = Stream.emits(List(tomHolland, galGodot, ezraMiller))
  val jlActors: Stream[Pure, Actor] = Stream(henryCavil, galGodot, ezraMiller, benFisher, rayHardy)
  val liftedJlActors: Stream[IO, Actor] = jlActors.covary[IO]
  val evalMappedJlActors: Stream[IO, Unit] = jlActors.evalMap(IO.println)
  val evalTappedJlActors: Stream[IO, Actor] = jlActors.evalTap(IO.println)
  val avengersActors: Stream[Pure, Actor] = Stream.chunk(Chunk.array(Array(tomHolland, galGodot, ezraMiller)))
  val avengersActorsByFirstName: Stream[Pure, Map[String, List[Actor]]] = avengersActors.fold(Map.empty[String, List[Actor]]) { (map, actor) =>
    map + (actor.firstName -> (actor :: map.getOrElse(actor.firstName, Nil)))
  }
  val dcAndMarvelSuperheroes: Stream[Pure, Actor] = jlActors ++ avengersActors

  def jlActorStream[F[_] : MonadThrow]: Stream[F, Actor] = jlActors.covary[F]
  val jlActorsEffectfulList: IO[List[Actor]] = liftedJlActors.compile.toList
  def save(a:Actor) =  IO {
    println(s"Saving actor $a");
    Thread.sleep(1000);
    println("Finished")
    a.id
  }
  val savedJlActors: Stream[IO, Int] = jlActors.evalMap(save)
  val savingTomHolland: Stream[IO, Unit] = Stream.eval {
    save(tomHolland).void
  }
  savingTomHolland.compile.drain.unsafeRunSync() //import cats.effect.unsafe.implicits.global
  //oder in IOApp .. def run..:
  savingTomHolland.compile.drain.as(ExitCode.Success)
  val fromActorToStringPipe: Pipe[IO, Actor, String] = in =>
    in.map(actor => s"${actor.firstName} ${actor.lastName}")

  def toConsole[T]: Pipe[IO, T, Unit] = in =>
    in.evalMap(str => IO.println(str))

  val stringNamesOfJlActors: Stream[IO, Unit] =
    jlActors.through(fromActorToStringPipe).through(toConsole)

  val attemptedSavedJlActors: Stream[IO, Either[Throwable, Int]] = savedJlActors.attempt
  attemptedSavedJlActors.evalMap {
    case Left(error) => IO.println(s"Error: $error")
    case Right(id) => IO.println(s"Saved actor with id: $id")
  }

  val acquire = IO {
    println(s"Acquiring connection to the database: 17")
    17
  }
  val release = (conn: Int) =>
    IO.println(s"Releasing connection to the database: $conn")

/*
  vgl. https://blog.rockthejvm.com/fs2/
*/
  //!!!bracket -> Resource im Stream-Kontext !!!!
  val writeResourceStreamAsyncEx = Stream.bracket(acquire )(release).flatMap(conn =>  writeStreamAsyncEx)

  test("run  Stream parEvalMapUnordered  chunkN ") {
    runStream(writeStreamAsync)
  }

  test("2run  Stream parEvalMapUnordered  chunkN ") {
    runStream(writeResourceStreamAsyncEx)
  }

  test("2run  Stream parEvalMapUnordered  chunkN ") {
    val p = ("1", "2")
    println(p.(1))
  }
  val tomHollandActorPull: Pull[Pure, Actor, Unit] = Pull.output1(tomHolland)
  val tomHollandActorStream: Stream[Pure, Actor] = tomHollandActorPull.stream //wenn R = Unit -> stream
  val spiderMenActorPull: Pull[Pure, Actor, Unit] =
    tomHollandActorPull >> Pull.output1(galGodot) >> Pull.output1(ezraMiller)
  val avengersActorsPull: Pull[Pure, Actor, Unit] = avengersActors.pull.echo
  val unconsAvengersActors: Pull[Pure, Nothing, Option[(Chunk[Actor], Stream[Pure, Actor])]] =
    avengersActors.pull.uncons
  val uncons1AvengersActors: Pull[Pure, Nothing, Option[(Actor, Stream[Pure, Actor])]] =
    avengersActors.pull.uncons1

  def takeByName(name: String): Pipe[IO, Actor, Actor] = {
    def go(s: Stream[IO, Actor]): Pull[IO, Actor, Unit] =
      s.pull.uncons1.flatMap {
        case Some((hd, tl)) =>
          if (hd.firstName == name) Pull.output1(hd) >> go(tl)
          else go(tl)
        case None => Pull.done
      }
    in => go(in).stream
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







