import BusinessError.RandomError
import cats.effect.MonadCancelThrow
import cats.effect.std.Random
import cats.{Monad, MonadThrow}

import scala.util.control.NoStackTrace

sealed trait BusinessError extends NoStackTrace
object BusinessError {
  type RandomError = RandomError.type
  case object RandomError extends BusinessError
}

trait Categories[F[_]] {
  def findAll: F[List[String]]
}

object Categories {
  def make[F[_] :  MonadCancelThrow :    Random]: Categories[F] =
    new Categories[F] {
      def findAll: F[List[String]] = {
        val r =  Random[F].nextInt
        r.flatMap {
          case n if n > 100 => List.empty[String].pure[F]
          case _ => RandomError.raiseError[F, List[String]]
        }
      }
    }
}