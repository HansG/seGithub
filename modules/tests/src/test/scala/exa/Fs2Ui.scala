package exa

import cats.effect._
import cats.effect.unsafe.implicits.global
import cats.implicits._
import fs2.Chunk.Queue
import fs2._
import javafx.application._
import javafx.beans.value.WritableValue
import javafx.scene._
import javafx.scene.control._
import javafx.scene.input._
import javafx.scene.layout._
import javafx.stage._

import scala.concurrent.ExecutionContext
import scala.util.Try

class Fs2Ui extends Application {
  override def start(primaryStage: Stage): Unit = {
    implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)
    implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
    implicit val blocker = Blocker.liftExecutionContext(scala.concurrent.ExecutionContext.global)


    new Logic[IO]().run(primaryStage).start.unsafeRunSync()
  }

  class Logic[F[_]: ConcurrentEffect: ContextShift: Timer] {
    import Fs2Ui._

    import java.time.{Duration, Instant}
    import java.util.concurrent.TimeUnit.MILLISECONDS

    def run(primaryStage: Stage): F[Unit] = for {
      v <- initializeUi(primaryStage)
      View(input, feedback) = v

      _ <- Stream(input).covary[F]
        .through(typedChars)
        .through(processInput)
        .through(displayFeedback(feedback.textProperty))
        .compile.drain
    } yield ()

    private def initializeUi(primaryStage: Stage): F[View] = updateUi {
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

    private def processInput: Pipe[F, TypedChar, Feedback] = for {
      typed <- _
      _ <- Stream.eval(ContextShift[F].shift)
      res <- Stream.eval { time(processSingle(typed)) }
      (d, Feedback(str)) = res
    } yield Feedback(s"$str in [$d]")

    private def displayFeedback(value: WritableValue[String]): Pipe[F, Feedback, Unit] =
      _.map { case Feedback(str) => str } through updateValue(value)

    private def time[A](f: F[A]): F[(Duration, A)] = {
      val now = Timer[F].clock.monotonic(MILLISECONDS).map(Instant.ofEpochMilli)
      for {
        start <- now
        a <- f
        stop <- now
        d = Duration.between(start, stop)
      } yield (d, a)
    }

    private val processSingle: TypedChar => F[Feedback] = {
      import scala.concurrent.duration._
      import scala.util.Random

      val prng = new Random()
      def randomDelay: F[Unit] = Timer[F].sleep { (250 + prng.nextInt(750)).millis }

      c => randomDelay *> Sync[F].delay(Feedback(s"processed $c"))
    }
  }
}

object Fs2Ui {
  case class View(input: TextField, feedback: Label)

  case class TypedChar(value: String)
  case class Feedback(value: String)

  private def typedChars[F[_]: ConcurrentEffect]: Pipe[F, Node, TypedChar] = for {
    node <- _
    q <- Stream.eval(Sync[F].delay(Queue.empty[KeyEvent]))
    _ <- Stream.eval(Sync[F].delay {
      node.setOnKeyTyped { evt => (q.chunks.enqueue(evt)) }
    })
    keyEvent <- q.chunks.dequeue
  } yield TypedChar(keyEvent.getCharacter)

  private def updateValue[F[_]: Async, A](value: WritableValue[A]): Pipe[F, A, Unit] = for {
    a <- _
    _ <- Stream.eval(updateUi(value setValue a))
  } yield ()

  private def updateUi[F[_]: Async, A](action: => A): F[A] =
    Async[F].async[A] { cb =>
      Platform.runLater { () =>
        cb(Try(action).toEither)
      }
    }
}