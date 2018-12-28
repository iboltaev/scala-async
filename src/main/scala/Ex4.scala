import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._

object Ex4 {
  implicit val asMonad = new Ex3.AsyncStreamMonad

  // this was rewrited from 'foldLeft' usage because of future
  // switching to CancellableFutures, not supporting 'flatten'
  def foreach[A, S](stream: Ex3.AsyncStream[A])(f: A => Ex2.FState[S, Any]): Ex2.FState[S, Any] = {
    implicit val fsMonad = new Ex2.FStateMonad[S]
    def impl(d: Ex3.AsyncStream[A], acc: Future[S]): Future[S] = {
      // Future[S]
      d.data.flatMap { pair =>
        if (pair eq null) acc
        else impl(pair.second, acc.flatMap { s2 =>
          f(pair.first)(s2).map(_._2)
        })
      }
    }

    Ex2.FState(s => impl(stream, Future(s)).map{s2 => (null, s2)})
  }

  def isEmpty[A, S](stream: Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] =
    Ex2.FState(s => stream.data.map { pair =>
      (pair eq null, s)
    })

  def isEmpty[A, S : Ex2.FStateMonad](f: S => Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] = {
    val m = implicitly[Ex2.FStateMonad[S]]
    m.fconds { s =>
      isEmpty(f(s))
    }
  }

  def notEmpty[A, S](stream: Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] =
    Ex2.FState(s => stream.data.map { pair =>
      (!(pair eq null), s)
    })

  def notEmpty[A, S : Ex2.FStateMonad](f: S => Ex3.AsyncStream[A]): Ex2.FState[S, Boolean] = {
    val m = implicitly[Ex2.FStateMonad[S]]
    m.fconds { s =>
      notEmpty(f(s))
    }
  }

  def get[A, S](stream: Ex3.AsyncStream[A]): Ex2.FState[S, (A, Ex3.AsyncStream[A])] =
    Ex2.FState(s => stream.data.map  {pair =>
      ((pair.first, pair.second), s)
    })

  def genAsyncStream[S,A](start: S)(gen: Ex2.FState[S, A]) =
    Ex3.genAsyncStream(start)(gen.f)
}
