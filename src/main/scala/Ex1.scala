import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

object Ex1 {
  class FState[S, A](func: S => Future[(A, S)]) {
    def apply(s: S) = func(s)

    def flatMap[B](f: A => FState[S, B]): FState[S, B] = new FState[S, B](
      (s: S) =>
        func(s).flatMap { pair =>
          f(pair._1)(pair._2)
        }
    )

    def map[B](f: A => B): FState[S, B] = flatMap { a: A =>
      FState.unit(f(a))
    }
  }

  object FState {
    def apply[S, A](f: S => Future[(A, S)]) = new FState[S, A](f)
    def unit[S, A](a: => A) = new FState[S, A]((s: S) => Future { (a, s) })
  }
}
