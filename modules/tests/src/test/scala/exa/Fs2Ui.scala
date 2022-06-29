package exa

import cats.effect._
import cats.effect.std.Queue
import cats.effect.unsafe.implicits.global
import cats.implicits._
import fs2._
import javafx.application._
import javafx.beans.value.WritableValue
import javafx.scene.{Node, _}
import javafx.scene.control._
import javafx.scene.input._
import javafx.scene.layout._
import javafx.stage._

import scala.concurrent.ExecutionContext
import scala.util.Try
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.ExecutionContext.global

class Fs2Ui extends Application {

  override def start(primaryStage: Stage): Unit = {
    implicit val cs: IO[ExecutionContext] = IO.executionContext //(ExecutionContext.global)
   // implicit val timer  = IO.timer(ExecutionContext.global)
  //  implicit val blocker = Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.global)


    new Logic[IO]().run(primaryStage).start.unsafeRunSync()
  }

  class Logic[IO[_]: Concurrent ] {//: Logger
    import Fs2Ui._

    import java.time.{Duration, Instant}
    import java.util.concurrent.TimeUnit.MILLISECONDS

  /*  def run(primaryStage: Stage): F[Unit] = for {
      v <- initializeUi(primaryStage)
      View(input, feedback) = v

      _ <- Stream(input).covary[IO]
        .through( typedChars )
        .through(processInput)
        .through(displayFeedback(feedback.textProperty))
        .compile.drain
    } yield ()*/


    def run(primaryStage: Stage): IO[Unit] = for {
      v <- initializeUi(primaryStage)
      View(input, feedback) = v

      evSrc <- Stream.eval(Fs2Ui.typedCharsSource(input))
                .flatMap( evs => evs.read )
                .map(tc =>   displayFeedback(feedback.textProperty))
        .compile.drain
    } yield ()

    def run1(primaryStage: Stage): IO[EventSource[IO]] = {
       initializeUi(primaryStage).flatMap {
       v =>  {
         val View(input, feedback) = v
         Fs2Ui.typedCharsSource(input)
       }
    }
    }


    private def initializeUi(primaryStage: Stage): IO[View] = updateUi {
      val input = new TextField()
      input.setPrefWidth(300)
      val feedback = new Label("...")

      val vbox = new VBox(input, feedback)
      val root = new StackPane(vbox)
      val scene = new Scene(root)

      primaryStage.setScene(scene)
      primaryStage.show()

      View(input, feedback)
    }

    private def processInput: Pipe[IO, TypedChar, Feedback] = for {
      typed <- _
      res <- Stream.eval { time(processSingle(typed)) }
      (d, Feedback(str)) = res
    } yield Feedback(s"$str in [$d]")

    private def processInput_1: Pipe[IO, TypedChar, Feedback] = for {
      typed <- _
   //   _ <- Stream.eval(ContextShift[IO].shift)
      res <- Stream.eval { time(processSingle(typed)) }
      (d, Feedback(str)) = res
    } yield Feedback(s"$str in [$d]")

    private def displayFeedback(value: WritableValue[String]): Pipe[IO, Feedback, Unit] =
      _.map { case Feedback(str) => str } through updateValue(value)

    private def time[A](f: IO[A]): IO[(Duration, A)] = {
      val now = IO.monotonic.map(d => Instant.ofEpochMilli(d.toMillis) )
      for {
        start <- now
        a <- f
        stop <- now
        d = Duration.between(start, stop)
      } yield (d, a)
    }

    private val processSingle: TypedChar => IO[Feedback] = {c =>
    import scala.concurrent.duration._
      import scala.util.Random

      val prng = new Random()
      def randomDelay: IO[Unit] = IO.sleep( { (250 + prng.nextInt(750)).millis })

      randomDelay *>  IO.delay(Feedback(s"processed $c"))
    }
  }
}

object Fs2Ui {
  case class View(input: TextField, feedback: Label)

  case class TypedChar(value: String)
  case class Feedback(value: String)

  /*
  private def typedChars[IO[_]: Concurrent]: Pipe[IO, Node, TypedChar] = for {
    node <- _
    q <- Stream.eval(Sync[IO].delay(Queue.empty[KeyEvent]))
    _ <- Stream.eval(Sync[IO].delay {
      node.setOnKeyTyped { evt => (q.chunks.enqueue(evt)) }
    })
    keyEvent <- q.chunks.dequeue
  } yield TypedChar(keyEvent.getCharacter)
*/
  def typedCharsSource[IO[_]: Concurrent](node : Node) = {
    for {
      qu <- Queue.bounded[IO, KeyEvent](20)
    } yield new EventSource(node, qu)
  }

  class EventSource[IO[_]: Concurrent](node : Node, qu : Queue[IO, KeyEvent]){
    node.setOnKeyTyped { evt => (qu.offer(evt)) }
    def read: Stream[IO, TypedChar] = Stream.fromQueueUnterminated(qu).map( (ke:KeyEvent) =>  TypedChar(ke.getCharacter))
  }

  private def updateValue[IO[_]: Async, A](value: WritableValue[A]): Pipe[IO, A, Unit] = for {
    a <- _
    _ <- Stream.eval(updateUi(value setValue a))
  } yield ()

  private def updateUi[A](action: => A): IO[A] =
    Async[IO].async[A] { cb =>
      Platform.runLater { () =>
        cb(Try(action).toEither)
      }
      IO(Some(IO(())))
    }
}